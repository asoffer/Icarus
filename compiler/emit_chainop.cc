#include <utility>

#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "base/guarded.h"
#include "compiler/compiler.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/value/enum_and_flags.h"
#include "ir/value/value.h"
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
// TODO this should early exit if the types aren't equal. Should also accept
// Typed<Value, Array>
static ir::Value ArrayCompare(Compiler *compiler, type::Array const *lhs_type,
                              ir::Value const &lhs_ir,
                              type::Array const *rhs_type,
                              ir::Value const &rhs_ir, bool equality) {
  auto &bldr  = compiler->builder();
  auto &funcs = equality ? eq_funcs : ne_funcs;
  auto handle = funcs.lock();

  auto [iter, success] = (*handle)[lhs_type].emplace(rhs_type, nullptr);
  if (success) {
    auto const *fn_type = type::Func(
        {core::AnonymousParam(type::QualType::NonConstant(type::Ptr(lhs_type))),
         core::AnonymousParam(
             type::QualType::NonConstant(type::Ptr(rhs_type)))},
        {type::Bool});
    ir::NativeFn fn = compiler->AddFunc(
        fn_type, fn_type->params().Transform([](type::QualType q) {
          return type::Typed<ast::Declaration const *>(nullptr, q.type());
        }));

    ICARUS_SCOPE(ir::SetCurrent(fn)) {
      bldr.CurrentBlock() = fn->entry();

      auto *equal_len_block = bldr.AddBlock();
      auto *true_block      = bldr.AddBlock();
      auto *false_block     = bldr.AddBlock();
      auto *phi_block       = bldr.AddBlock();
      auto *body_block      = bldr.AddBlock();
      auto *incr_block      = bldr.AddBlock();

      bldr.CondJump(bldr.Eq(lhs_type->length(), rhs_type->length()),
                    equal_len_block, false_block);

      bldr.CurrentBlock() = true_block;
      bldr.SetRet(0, true);
      bldr.ReturnJump();

      bldr.CurrentBlock() = false_block;
      bldr.SetRet(0, false);
      bldr.ReturnJump();

      bldr.CurrentBlock() = equal_len_block;
      auto lhs_start =
          compiler->builder().Index(Ptr(lhs_type), ir::Reg::Arg(0), 0);
      auto rhs_start =
          compiler->builder().Index(Ptr(rhs_type), ir::Reg::Arg(1), 0);
      auto lhs_end = bldr.PtrIncr(lhs_start, lhs_type->length(),
                                  Ptr(rhs_type->data_type()));
      bldr.UncondJump(phi_block);

      bldr.CurrentBlock() = phi_block;

      ir::Reg lhs_phi_reg = bldr.CurrentGroup()->Reserve();
      ir::Reg rhs_phi_reg = bldr.CurrentGroup()->Reserve();

      bldr.CondJump(bldr.Eq(ir::RegOr<ir::Addr>(lhs_phi_reg), lhs_end),
                    true_block, body_block);

      bldr.CurrentBlock() = body_block;
      // TODO what if data type is an array?
      bldr.CondJump(bldr.Eq(bldr.Load<ir::Addr>(lhs_phi_reg),
                            bldr.Load<ir::Addr>(rhs_phi_reg)),
                    incr_block, false_block);

      bldr.CurrentBlock() = incr_block;
      auto lhs_incr = bldr.PtrIncr(lhs_phi_reg, 1, Ptr(lhs_type->data_type()));
      auto rhs_incr = bldr.PtrIncr(rhs_phi_reg, 1, Ptr(rhs_type->data_type()));
      bldr.UncondJump(phi_block);

      bldr.Phi<ir::Addr>(lhs_phi_reg, {equal_len_block, incr_block},
                         {lhs_start, lhs_incr});
      bldr.Phi<ir::Addr>(rhs_phi_reg, {equal_len_block, incr_block},
                         {rhs_start, rhs_incr});
    }
  }

  ir::OutParams outs = compiler->builder().OutParams({type::Bool});
  auto result        = outs[0];
  bldr.Call(ir::Fn{iter->second}, iter->second->type(), {lhs_ir, rhs_ir},
            std::move(outs));
  return ir::Value(result);
}

static ir::RegOr<bool> EmitChainOpPair(Compiler *compiler,
                                       ast::ComparisonOperator const *chain_op,
                                       size_t index, ir::Value const &lhs_ir,
                                       ir::Value const &rhs_ir) {
  auto &bldr     = compiler->builder();
  auto *lhs_type = compiler->type_of(chain_op->exprs()[index]);
  auto *rhs_type = compiler->type_of(chain_op->exprs()[index + 1]);
  auto op        = chain_op->ops()[index];

  if (lhs_type->is<type::Array>() and rhs_type->is<type::Array>()) {
    return ArrayCompare(compiler, &lhs_type->as<type::Array>(), lhs_ir,
                        &rhs_type->as<type::Array>(), rhs_ir,
                        op == frontend::Operator::Eq)
        .get<ir::RegOr<bool>>();
  } else if (lhs_type->is<type::Struct>() or rhs_type->is<type::Struct>()) {
    NOT_YET();

  } else {
    switch (op) {
      case frontend::Operator::Lt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return bldr.Lt(lhs_ir.get<ir::RegOr<T>>(),
                         rhs_ir.get<ir::RegOr<T>>());
        });
      case frontend::Operator::Le:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return bldr.Le(lhs_ir.get<ir::RegOr<T>>(),
                         rhs_ir.get<ir::RegOr<T>>());
        });
      case frontend::Operator::Eq:
        if (lhs_type == type::Block) {
          auto val1 = lhs_ir.get<ir::RegOr<ir::BlockDef *>>();
          auto val2 = rhs_ir.get<ir::RegOr<ir::BlockDef *>>();
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
              return bldr.Eq(lhs_ir.get<ir::RegOr<T>>(),
                             rhs_ir.get<ir::RegOr<T>>());
            });
      case frontend::Operator::Ne:
        if (lhs_type == type::Block) {
          auto val1 = lhs_ir.get<ir::RegOr<ir::BlockDef *>>();
          auto val2 = rhs_ir.get<ir::RegOr<ir::BlockDef *>>();
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
              return bldr.Ne(lhs_ir.get<ir::RegOr<T>>(),
                             rhs_ir.get<ir::RegOr<T>>());
            });
      case frontend::Operator::Ge:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return bldr.Ge(lhs_ir.get<ir::RegOr<T>>(),
                         rhs_ir.get<ir::RegOr<T>>());
        });
      case frontend::Operator::Gt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto tag) {
          using T = typename decltype(tag)::type;
          return bldr.Gt(lhs_ir.get<ir::RegOr<T>>(),
                         rhs_ir.get<ir::RegOr<T>>());
        });
        // TODO case frontend::Operator::And: cmp = lhs_ir; break;
      default: UNREACHABLE();
    }
  }
}

ir::Value Compiler::EmitValue(ast::ComparisonOperator const *node) {
  auto *t = type_of(node);
  if (node->ops().size() == 1) {
    auto lhs_ir = EmitValue(node->exprs()[0]);
    auto rhs_ir = EmitValue(node->exprs()[1]);
    return ir::Value(EmitChainOpPair(this, node, 0, lhs_ir, rhs_ir));

  } else {
    std::vector<ir::BasicBlock const *> phi_blocks;
    std::vector<ir::RegOr<bool>> phi_values;
    auto lhs_ir      = EmitValue(node->exprs().front());
    auto *land_block = builder().AddBlock();
    for (size_t i = 0; i + 1 < node->ops().size(); ++i) {
      auto rhs_ir = EmitValue(node->exprs()[i + 1]);
      auto cmp    = EmitChainOpPair(this, node, i, lhs_ir, rhs_ir);

      phi_blocks.push_back(builder().CurrentBlock());
      phi_values.push_back(false);
      auto *next_block = builder().AddBlock();
      builder().CondJump(cmp, next_block, land_block);
      builder().CurrentBlock() = next_block;
      lhs_ir                   = std::move(rhs_ir);
    }

    // Once more for the last element, but don't do a conditional jump.
    auto rhs_ir = EmitValue(node->exprs().back());
    phi_blocks.push_back(builder().CurrentBlock());
    phi_values.push_back(
        EmitChainOpPair(this, node, node->exprs().size() - 2, lhs_ir, rhs_ir));
    builder().UncondJump(land_block);

    builder().CurrentBlock() = land_block;

    return ir::Value(
        builder().Phi<bool>(std::move(phi_blocks), std::move(phi_values)));
  }
}

}  // namespace compiler
