#include "ast/chainop.h"

#include <numeric>

#include "core/fn_args.h"
#include "ast/overload_set.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "misc/context.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"

namespace ir {
RegisterOr<type::Type const *> Variant(
    std::vector<RegisterOr<type::Type const *>> const &vals);
}  // namespace ir

namespace ast {
namespace {

ir::RegisterOr<bool> EmitChainOpPair(ast::ChainOp *chain_op, size_t index,
                                     ir::Results const &lhs_ir,
                                     ir::Results const &rhs_ir, Context *ctx) {
  auto *lhs_type = ctx->type_of(chain_op->exprs[index].get());
  auto *rhs_type = ctx->type_of(chain_op->exprs[index + 1].get());
  auto op        = chain_op->ops[index];

  if (lhs_type->is<type::Array>() && rhs_type->is<type::Array>()) {
    using ::matcher::Eq;
    ASSERT(op, Eq(frontend::Operator::Eq) || Eq(frontend::Operator::Ne));
    return type::Array::Compare(&lhs_type->as<type::Array>(), lhs_ir,
                                &rhs_type->as<type::Array>(), rhs_ir,
                                op == frontend::Operator::Eq, ctx)
        .get<bool>(0);
  } else if (lhs_type->is<type::Struct>() || rhs_type->is<type::Struct>()) {
    auto results =
        ASSERT_NOT_NULL(
            ctx->dispatch_table(reinterpret_cast<Expression *>(
                reinterpret_cast<uintptr_t>(chain_op->exprs[index].get()) |
                0x1)))
            ->EmitCall(
                core::FnArgs<std::pair<Expression const *, ir::Results>>(
                    {std::pair(chain_op->exprs[index].get(), lhs_ir),
                     std::pair(chain_op->exprs[index + 1].get(), rhs_ir)},
                    {}),
                ctx);
    ASSERT(results.size() == 1u);
    return results.get<bool>(0);

  } else {
    switch (op) {
      case frontend::Operator::Lt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Lt(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Le:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Le(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Eq:
        if (lhs_type == type::Block || lhs_type == type::OptBlock ||
            lhs_type == type::RepBlock) {
          auto val1 = lhs_ir.get<ir::BlockSequence>(0);
          auto val2 = rhs_ir.get<ir::BlockSequence>(0);
          if (!val1.is_reg_ && !val2.is_reg_) { return val1.val_ == val2.val_; }
        }
        return type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t,
                                uint8_t, uint16_t, uint32_t, uint64_t, float,
                                double, type::Type const *, ir::EnumVal,
                                ir::FlagsVal, ir::Addr>(
            lhs_type, [&](auto type_holder) {
              using T = typename decltype(type_holder)::type;
              return ir::Eq(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
            });
      case frontend::Operator::Ne:
        if (lhs_type == type::Block || lhs_type == type::OptBlock ||
            lhs_type == type::RepBlock) {
          auto val1 = lhs_ir.get<ir::BlockSequence>(0);
          auto val2 = rhs_ir.get<ir::BlockSequence>(0);
          if (!val1.is_reg_ && !val2.is_reg_) { return val1.val_ == val2.val_; }
      }
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                type::Type const *, ir::EnumVal, ir::FlagsVal,
                                ir::Addr>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Ne(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Ge:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Ge(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
      case frontend::Operator::Gt:
        return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                                uint16_t, uint32_t, uint64_t, float, double,
                                ir::FlagsVal>(lhs_type, [&](auto type_holder) {
          using T = typename decltype(type_holder)::type;
          return ir::Gt(lhs_ir.get<T>(0), rhs_ir.get<T>(0));
        });
        // TODO case frontend::Operator::And: cmp = lhs_ir; break;
      default: UNREACHABLE();
    }
  }
}

}  // namespace

std::string ChainOp::to_string(size_t n) const {
  std::stringstream ss;
  ss << "(";
  for (size_t i = 0; i < ops.size(); ++i) {
    ss << exprs[i]->to_string(n);
    switch (ops[i]) {
      case frontend::Operator::Or: ss << " | "; break;
      case frontend::Operator::Xor: ss << " ^ "; break;
      case frontend::Operator::And: ss << " & "; break;
      case frontend::Operator::Lt: ss << " < "; break;
      case frontend::Operator::Le: ss << " <= "; break;
      case frontend::Operator::Eq: ss << " == "; break;
      case frontend::Operator::Ne: ss << " != "; break;
      case frontend::Operator::Ge: ss << " >= "; break;
      case frontend::Operator::Gt: ss << " > "; break;
      default: UNREACHABLE();
    }
  }
  ss << exprs.back()->to_string(n) << ")";
  return ss.str();
}

ir::Results ChainOp::EmitIr(Context *ctx) {
  auto *t = ctx->type_of(this);
  if (ops[0] == frontend::Operator::Xor) {
    if (t == type::Bool) {
      return ir::Results{std::accumulate(
          exprs.begin(), exprs.end(), ir::RegisterOr<bool>(false),
          [&](ir::RegisterOr<bool> acc, auto &expr) {
            return ir::XorBool(acc, expr->EmitIr(ctx).template get<bool>(0));
          })};
    } else if (t->is<type::Flags>()) {
      return ir::Results{
          std::accumulate(exprs.begin(), exprs.end(),
                          ir::RegisterOr<ir::FlagsVal>(ir::FlagsVal{0}),
                          [&](ir::RegisterOr<ir::FlagsVal> acc, auto &expr) {
                            return ir::XorFlags(
                                &t->as<type::Flags>(), acc,
                                expr->EmitIr(ctx).template get<ir::FlagsVal>(
                                    0));
                          })};
    } else {
      UNREACHABLE();
    }

  } else if (ops[0] == frontend::Operator::Or && t->is<type::Flags>()) {
    auto iter = exprs.begin();
    auto val  = (*iter)->EmitIr(ctx).get<ir::FlagsVal>(0);
    while (++iter != exprs.end()) {
      val = ir::OrFlags(&t->as<type::Flags>(), val,
                        (*iter)->EmitIr(ctx).get<ir::FlagsVal>(0));
    }
    return ir::Results{val};
  } else if (ops[0] == frontend::Operator::And && t->is<type::Flags>()) {
    auto iter = exprs.begin();
    auto val  = (*iter)->EmitIr(ctx).get<ir::FlagsVal>(0);
    while (++iter != exprs.end()) {
      val = ir::AndFlags(&t->as<type::Flags>(), val,
                         (*iter)->EmitIr(ctx).get<ir::FlagsVal>(0));
    }
    return ir::Results{val};
  } else if (ops[0] == frontend::Operator::Or && t == type::Type_) {
    // TODO probably want to check that each expression is a type? What if I
    // overload | to take my own stuff and have it return a type?
    std::vector<ir::RegisterOr<type::Type const *>> args;
    args.reserve(exprs.size());
    for (const auto &expr : exprs) {
      args.push_back(expr->EmitIr(ctx).get<type::Type const *>(0));
    }
    auto reg_or_type = ir::Variant(args);
    return ir::Results{reg_or_type};
  } else if (ops[0] == frontend::Operator::Or &&
             (t == type::Block || t == type::OptBlock)) {
    ir::BlockSequence seq;
    for (auto &expr : exprs) {
      auto reg_or_seq = expr->EmitIr(ctx).get<ir::BlockSequence>(0);
      ASSERT(reg_or_seq.is_reg_ == false);
      seq |= reg_or_seq.val_;
    }
    return ir::Results{seq};
  } else if (ops[0] == frontend::Operator::And ||
             ops[0] == frontend::Operator::Or) {
    auto land_block = ir::CompiledFn::Current->AddBlock();

    absl::flat_hash_map<ir::BlockIndex, ir::RegisterOr<bool>> phi_args;
    bool is_or = (ops[0] == frontend::Operator::Or);
    for (size_t i = 0; i + 1 < exprs.size(); ++i) {
      auto val = exprs[i]->EmitIr(ctx).get<bool>(0);

      auto next_block = ir::CompiledFn::Current->AddBlock();
      ir::CondJump(val, is_or ? land_block : next_block,
                   is_or ? next_block : land_block);
      phi_args.emplace(ir::BasicBlock::Current, is_or);

      ir::BasicBlock::Current = next_block;
    }

    phi_args.emplace(ir::BasicBlock::Current,
                     exprs.back()->EmitIr(ctx).get<bool>(0));
    ir::UncondJump(land_block);

    ir::BasicBlock::Current = land_block;

    return ir::Results{ir::MakePhi<bool>(ir::Phi(type::Bool), phi_args)};

  } else {
    if (ops.size() == 1) {
      auto lhs_ir = exprs[0]->EmitIr(ctx);
      auto rhs_ir = exprs[1]->EmitIr(ctx);
      return ir::Results{EmitChainOpPair(this, 0, lhs_ir, rhs_ir, ctx)};

    } else {
      absl::flat_hash_map<ir::BlockIndex, ir::RegisterOr<bool>> phi_args;
      auto lhs_ir     = exprs.front()->EmitIr(ctx);
      auto land_block = ir::CompiledFn::Current->AddBlock();
      for (size_t i = 0; i < ops.size() - 1; ++i) {
        auto rhs_ir = exprs[i + 1]->EmitIr(ctx);
        auto cmp    = EmitChainOpPair(this, i, lhs_ir, rhs_ir, ctx);

        phi_args.emplace(ir::BasicBlock::Current, false);
        auto next_block = ir::CompiledFn::Current->AddBlock();
        ir::CondJump(cmp, next_block, land_block);
        ir::BasicBlock::Current = next_block;
        lhs_ir                  = std::move(rhs_ir);
      }

      // Once more for the last element, but don't do a conditional jump.
      auto rhs_ir = exprs.back()->EmitIr(ctx);
      phi_args.emplace(
          ir::BasicBlock::Current,
          EmitChainOpPair(this, exprs.size() - 2, lhs_ir, rhs_ir, ctx));
      ir::UncondJump(land_block);

      ir::BasicBlock::Current = land_block;

      return ir::Results{ir::MakePhi<bool>(ir::Phi(type::Bool), phi_args)};
    }
  }
}

}  // namespace ast
