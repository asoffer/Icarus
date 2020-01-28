#include <utility>

#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "base/guarded.h"
#include "compiler/compiler.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/components.h"
#include "ir/results.h"
#include "ir/values.h"
#include "type/array.h"
#include "type/function.h"
#include "type/pointer.h"

namespace compiler {

static base::guarded<absl::flat_hash_map<
    type::Array const *,
    absl::flat_hash_map<type::Array const *, ir::CompiledFn *>>>
    eq_funcs;
static base::guarded<absl::flat_hash_map<
    type::Array const *,
    absl::flat_hash_map<type::Array const *, ir::CompiledFn *>>>
    ne_funcs;
// TODO this should early exit if the types aren't equal.
static ir::Results ArrayCompare(Compiler *compiler, type::Array const *lhs_type,
                                ir::Results const &lhs_ir,
                                type::Array const *rhs_type,
                                ir::Results const &rhs_ir, bool equality) {
  auto &bldr  = compiler->builder();
  auto &funcs = equality ? eq_funcs : ne_funcs;
  auto handle = funcs.lock();

  auto[iter, success] = (*handle)[lhs_type].emplace(rhs_type, nullptr);
  if (success) {
    auto const *fn_type =
        type::Func({core::AnonymousParam(type::Ptr(lhs_type)),
                    core::AnonymousParam(type::Ptr(rhs_type))},
                   {type::Bool});
    auto *fn = compiler->AddFunc(fn_type, fn_type->AnonymousFnParams());

    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      bldr.CurrentBlock() = fn->entry();

      auto *equal_len_block = bldr.AddBlock();
      auto *true_block      = bldr.AddBlock();
      auto *false_block     = bldr.AddBlock();
      auto *phi_block       = bldr.AddBlock();
      auto *body_block      = bldr.AddBlock();
      auto *incr_block      = bldr.AddBlock();

      bldr.CondJump(bldr.Eq(lhs_type->len, rhs_type->len), equal_len_block,
                    false_block);

      bldr.CurrentBlock() = true_block;
      ir::SetRet(0, true);
      bldr.ReturnJump();

      bldr.CurrentBlock() = false_block;
      ir::SetRet(0, false);
      bldr.ReturnJump();

      bldr.CurrentBlock() = equal_len_block;
      auto lhs_start      = ir::Index(Ptr(lhs_type), ir::Reg::Arg(0), 0);
      auto rhs_start      = ir::Index(Ptr(rhs_type), ir::Reg::Arg(1), 0);
      auto lhs_end =
          bldr.PtrIncr(lhs_start, lhs_type->len, Ptr(rhs_type->data_type));
      bldr.UncondJump(phi_block);

      bldr.CurrentBlock() = phi_block;

      ir::Reg lhs_phi_reg = bldr.CurrentGroup()->Reserve();
      ir::Reg rhs_phi_reg = bldr.CurrentGroup()->Reserve();

      bldr.CondJump(bldr.Eq(ir::RegOr<ir::Addr>(lhs_phi_reg), lhs_end),
                    true_block, body_block);

      bldr.CurrentBlock() = body_block;
      // TODO what if data type is an array?
      bldr.CondJump(bldr.Eq(ir::Load<ir::Addr>(lhs_phi_reg),
                            ir::Load<ir::Addr>(rhs_phi_reg)),
                    incr_block, false_block);

      bldr.CurrentBlock() = incr_block;
      auto lhs_incr = bldr.PtrIncr(lhs_phi_reg, 1, Ptr(lhs_type->data_type));
      auto rhs_incr = bldr.PtrIncr(rhs_phi_reg, 1, Ptr(rhs_type->data_type));
      bldr.UncondJump(phi_block);

      bldr.Phi<ir::Addr>(lhs_phi_reg, {equal_len_block, incr_block},
                         {lhs_start, lhs_incr});
      bldr.Phi<ir::Addr>(rhs_phi_reg, {equal_len_block, incr_block},
                         {rhs_start, rhs_incr});
    }
  }

  ir::OutParams outs = compiler->builder().OutParams({type::Bool});
  auto result        = outs[0];
  bldr.Call(ir::AnyFunc{iter->second}, iter->second->type_, {lhs_ir, rhs_ir},
            std::move(outs));
  return ir::Results{result};
}

static ir::RegOr<bool> EmitChainOpPair(Compiler *compiler,
                                       ast::ChainOp const *chain_op,
                                       size_t index, ir::Results const &lhs_ir,
                                       ir::Results const &rhs_ir) {
  auto &bldr     = compiler->builder();
  auto *lhs_type = compiler->type_of(chain_op->exprs()[index]);
  auto *rhs_type = compiler->type_of(chain_op->exprs()[index + 1]);
  auto op        = chain_op->ops()[index];

  if (lhs_type->is<type::Array>() and rhs_type->is<type::Array>()) {
    return ArrayCompare(compiler, &lhs_type->as<type::Array>(), lhs_ir,
                        &rhs_type->as<type::Array>(), rhs_ir,
                        op == frontend::Operator::Eq)
        .get<bool>(0);
  } else if (lhs_type->is<type::Struct>() or rhs_type->is<type::Struct>()) {
    auto results =
        ASSERT_NOT_NULL(
            compiler->dispatch_table(reinterpret_cast<ast::Expression *>(
                reinterpret_cast<uintptr_t>(chain_op->exprs()[index]) | 0x1)))
            ->EmitCall(
                compiler,
                core::FnArgs<std::pair<ast::Expression const *, ir::Results>>(
                    {std::pair(chain_op->exprs()[index], lhs_ir),
                     std::pair(chain_op->exprs()[index + 1], rhs_ir)},
                    {}));
    ASSERT(results.size() == 1u);
    return results.get<bool>(0);

  } else {
    switch (op) {
      case frontend::Operator::Lt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return bldr.Lt(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Le:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return bldr.Le(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Eq:
        if (lhs_type == type::Block) {
          auto val1 = lhs_ir.get<ir::BlockDef *>(0);
          auto val2 = rhs_ir.get<ir::BlockDef *>(0);
          if (not val1.is_reg() and not val2.is_reg()) {
            return val1.value() == val2.value();
          }
        }
        return type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t,
                                uint8_t, uint16_t, uint32_t, uint64_t, float,
                                double, type::Type const *, ir::EnumVal,
                                ir::FlagsVal, ir::Addr>(
            lhs_type, [&](auto tag) {
              using T = typename decltype(tag)::type;
              return bldr.Eq(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
            });
      case frontend::Operator::Ne:
        if (lhs_type == type::Block) {
          auto val1 = lhs_ir.get<ir::BlockDef *>(0);
          auto val2 = rhs_ir.get<ir::BlockDef *>(0);
          if (not val1.is_reg() and not val2.is_reg()) {
            return val1.value() == val2.value();
          }
        }
        return type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t,
                                uint8_t, uint16_t, uint32_t, uint64_t, float,
                                double, type::Type const *, ir::EnumVal,
                                ir::FlagsVal, ir::Addr>(
            lhs_type, [&](auto tag) {
              using T = typename decltype(tag)::type;
              return bldr.Ne(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
            });
      case frontend::Operator::Ge:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return bldr.Ge(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Gt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return bldr.Gt(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
        // TODO case frontend::Operator::And: cmp = lhs_ir; break;
      default: UNREACHABLE();
    }
  }
}

ir::Results Compiler::Visit(ast::ChainOp const *node, EmitValueTag) {
  auto *t = type_of(node);
  if (node->ops()[0] == frontend::Operator::Xor) {
    if (t == type::Bool) {
      return ir::Results{std::accumulate(
          node->exprs().begin(), node->exprs().end(), ir::RegOr<bool>(false),
          [&](ir::RegOr<bool> acc, auto *expr) {
            return builder().Ne(
                acc, Visit(expr, EmitValueTag{}).template get<bool>(0));
          })};
    } else if (t->is<type::Flags>()) {
      return ir::Results{std::accumulate(
          node->exprs().begin(), node->exprs().end(),
          ir::RegOr<ir::FlagsVal>(ir::FlagsVal{0}),
          [&](ir::RegOr<ir::FlagsVal> acc, auto *expr) {
            return builder().XorFlags(
                acc, Visit(expr, EmitValueTag{}).template get<ir::FlagsVal>(0));
          })};
    } else {
      UNREACHABLE();
    }

  } else if (node->ops()[0] == frontend::Operator::Or and
             t->is<type::Flags>()) {
    auto iter = node->exprs().begin();
    auto val  = Visit(*iter, EmitValueTag{}).get<ir::FlagsVal>(0);
    while (++iter != node->exprs().end()) {
      val = builder().OrFlags(
          val, Visit(*iter, EmitValueTag{}).get<ir::FlagsVal>(0));
    }
    return ir::Results{val};
  } else if (node->ops()[0] == frontend::Operator::And and
             t->is<type::Flags>()) {
    auto iter = node->exprs().begin();
    auto val  = Visit(*iter, EmitValueTag{}).get<ir::FlagsVal>(0);
    while (++iter != node->exprs().end()) {
      val = builder().AndFlags(
          val, Visit(*iter, EmitValueTag{}).get<ir::FlagsVal>(0));
    }
    return ir::Results{val};
  } else if (node->ops()[0] == frontend::Operator::Or and t == type::Type_) {
    // TODO probably want to check that each expression is a type? What if I
    // overload | to take my own stuff and have it return a type?
    std::vector<ir::RegOr<type::Type const *>> args;
    args.reserve(node->exprs().size());
    for (auto const *expr : node->exprs()) {
      args.push_back(Visit(expr, EmitValueTag{}).get<type::Type const *>(0));
    }
    auto reg_or_type = builder().Var(args);
    return ir::Results{reg_or_type};
  } else if (node->ops()[0] == frontend::Operator::Or and t == type::Block) {
    NOT_YET();
  } else if (node->ops()[0] == frontend::Operator::And or
             node->ops()[0] == frontend::Operator::Or) {
    auto *land_block = builder().AddBlock();

    std::vector<ir::BasicBlock const *> phi_blocks;
    std::vector<ir::RegOr<bool>> phi_results;
    bool is_or = (node->ops()[0] == frontend::Operator::Or);
    for (size_t i = 0; i + 1 < node->exprs().size(); ++i) {
      auto val = Visit(node->exprs()[i], EmitValueTag{}).get<bool>(0);

      auto *next_block = builder().AddBlock();
      builder().CondJump(val, is_or ? land_block : next_block,
                         is_or ? next_block : land_block);
      phi_blocks.push_back(builder().CurrentBlock());
      phi_results.push_back(is_or);

      builder().CurrentBlock() = next_block;
    }

    phi_blocks.push_back(builder().CurrentBlock());
    phi_results.push_back(
        Visit(node->exprs().back(), EmitValueTag{}).get<bool>(0));
    builder().UncondJump(land_block);

    builder().CurrentBlock() = land_block;

    return ir::Results{
        builder().Phi<bool>(std::move(phi_blocks), std::move(phi_results))};

  } else {
    if (node->ops().size() == 1) {
      auto lhs_ir = Visit(node->exprs()[0], EmitValueTag{});
      auto rhs_ir = Visit(node->exprs()[1], EmitValueTag{});
      return ir::Results{EmitChainOpPair(this, node, 0, lhs_ir, rhs_ir)};

    } else {
      std::vector<ir::BasicBlock const *> phi_blocks;
      std::vector<ir::RegOr<bool>> phi_values;
      auto lhs_ir      = Visit(node->exprs().front(), EmitValueTag{});
      auto *land_block = builder().AddBlock();
      for (size_t i = 0; i + 1 < node->ops().size(); ++i) {
        auto rhs_ir = Visit(node->exprs()[i + 1], EmitValueTag{});
        auto cmp    = EmitChainOpPair(this, node, i, lhs_ir, rhs_ir);

        phi_blocks.push_back(builder().CurrentBlock());
        phi_values.push_back(false);
        auto *next_block = builder().AddBlock();
        builder().CondJump(cmp, next_block, land_block);
        builder().CurrentBlock() = next_block;
        lhs_ir                   = std::move(rhs_ir);
      }

      // Once more for the last element, but don't do a conditional jump.
      auto rhs_ir = Visit(node->exprs().back(), EmitValueTag{});
      phi_blocks.push_back(builder().CurrentBlock());
      phi_values.push_back(EmitChainOpPair(this, node, node->exprs().size() - 2,
                                           lhs_ir, rhs_ir));
      builder().UncondJump(land_block);

      builder().CurrentBlock() = land_block;

      return ir::Results{
          builder().Phi<bool>(std::move(phi_blocks), std::move(phi_values))};
    }
  }
  UNREACHABLE();
}

}  // namespace compiler