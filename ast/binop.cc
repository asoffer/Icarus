#include "ast/binop.h"

#include "ast/comma_list.h"
#include "core/fn_args.h"
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "misc/context.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace ir {
RegisterOr<type::Type const *> Tup(
    std::vector<RegisterOr<type::Type const *>> const &entries);
}  // namespace ir

namespace ast {

std::string Binop::to_string(size_t n) const {
  std::stringstream ss;
  ss << "(" << lhs->to_string(n) << ")";
  switch (op) {
    case frontend::Operator::Arrow: ss << " -> "; break;
    case frontend::Operator::Add: ss << " + "; break;
    case frontend::Operator::Sub: ss << " - "; break;
    case frontend::Operator::Mul: ss << " * "; break;
    case frontend::Operator::Div: ss << " / "; break;
    case frontend::Operator::Mod: ss << " % "; break;
    case frontend::Operator::Assign: ss << " = "; break;
    case frontend::Operator::OrEq: ss << " |= "; break;
    case frontend::Operator::XorEq: ss << " ^= "; break;
    case frontend::Operator::AndEq: ss << " &= "; break;
    case frontend::Operator::AddEq: ss << " += "; break;
    case frontend::Operator::SubEq: ss << " -= "; break;
    case frontend::Operator::MulEq: ss << " *= "; break;
    case frontend::Operator::DivEq: ss << " /= "; break;
    case frontend::Operator::ModEq: ss << " %= "; break;
    case frontend::Operator::When: ss << " when "; break;
    default: UNREACHABLE();
  }
  ss << "(" << rhs->to_string(n) << ")";

  return ss.str();
}

ir::Results Binop::EmitIr(Context *ctx) {
  auto *lhs_type = ctx->type_of(lhs.get());
  auto *rhs_type = ctx->type_of(rhs.get());

  if (auto *dispatch_table = ctx->dispatch_table(this)) {
    // TODO struct is not exactly right. we really mean user-defined
    return dispatch_table->EmitCall(
        core::FnArgs<std::pair<Expression const *, ir::Results>>(
            {std::pair(lhs.get(), lhs->EmitIr(ctx)),
             std::pair(rhs.get(), rhs->EmitIr(ctx))},
            {}),
        ctx);
  }

  switch (op) {
    case frontend::Operator::Add: {
      auto lhs_ir = lhs->EmitIr(ctx);
      auto rhs_ir = rhs->EmitIr(ctx);
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::Results{ir::Add(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Sub: {
      auto lhs_ir = lhs->EmitIr(ctx);
      auto rhs_ir = rhs->EmitIr(ctx);
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::Results{ir::Sub(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Mul: {
      auto lhs_ir = lhs->EmitIr(ctx);
      auto rhs_ir = rhs->EmitIr(ctx);
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::Results{ir::Mul(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Div: {
      auto lhs_ir = lhs->EmitIr(ctx);
      auto rhs_ir = rhs->EmitIr(ctx);
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::Results{ir::Div(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Mod: {
      auto lhs_ir = lhs->EmitIr(ctx);
      auto rhs_ir = rhs->EmitIr(ctx);
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t,
                              uint16_t, uint32_t, uint64_t>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::Results{ir::Mod(lhs_ir.get<T>(0), rhs_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::Arrow: {
      // TODO ugly hack.
      std::vector<ir::RegisterOr<type::Type const *>> lhs_vals, rhs_vals;
      if (auto *l = lhs->if_as<CommaList>()) {
        for (auto &e : l->exprs_) {
          lhs_vals.push_back(e->EmitIr(ctx).get<type::Type const *>(0));
        }
      } else {
        lhs_vals.push_back(lhs->EmitIr(ctx).get<type::Type const *>(0));
      }
      if (auto *r = rhs->if_as<CommaList>()) {
        for (auto &e : r->exprs_) {
          rhs_vals.push_back(e->EmitIr(ctx).get<type::Type const *>(0));
        }
      } else {
        rhs_vals.push_back(rhs->EmitIr(ctx).get<type::Type const *>(0));
      }

      auto reg_or_type = ir::Arrow(ir::Tup(lhs_vals), ir::Tup(rhs_vals));
      return ir::Results{reg_or_type};
    } break;
    case frontend::Operator::Assign: {
      // TODO support splatting.
      auto lhs_lvals = lhs->EmitLVal(ctx);
      if (lhs_lvals.size() != 1) { NOT_YET(); }

      auto rhs_vals = rhs->EmitIr(ctx);
      lhs_type->EmitMoveAssign(rhs_type, rhs_vals, lhs_lvals[0], ctx);

      return ir::Results{};
    } break;
    case frontend::Operator::OrEq: {
      auto *this_type = ctx->type_of(this);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = lhs->EmitLVal(ctx)[0];
        ir::Store(ir::OrFlags(&this_type->as<type::Flags>(),
                              ir::Load<ir::FlagsVal>(lhs_lval, this_type),
                              rhs->EmitIr(ctx).get<ir::FlagsVal>(0)),
                  lhs_lval);
        return ir::Results{};
      }
      auto land_block = ir::CompiledFn::Current->AddBlock();
      auto more_block = ir::CompiledFn::Current->AddBlock();

      auto lhs_val       = lhs->EmitIr(ctx).get<bool>(0);
      auto lhs_end_block = ir::BasicBlock::Current;
      ir::CondJump(lhs_val, land_block, more_block);

      ir::BasicBlock::Current = more_block;
      auto rhs_val            = rhs->EmitIr(ctx).get<bool>(0);
      auto rhs_end_block      = ir::BasicBlock::Current;
      ir::UncondJump(land_block);

      ir::BasicBlock::Current = land_block;

      return ir::Results{
          ir::MakePhi<bool>(ir::Phi(type::Bool),
                            {{lhs_end_block, true}, {rhs_end_block, rhs_val}})};
    } break;
    case frontend::Operator::AndEq: {
      auto *this_type = ctx->type_of(this);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = lhs->EmitLVal(ctx)[0];
        ir::Store(ir::AndFlags(&this_type->as<type::Flags>(),
                               ir::Load<ir::FlagsVal>(lhs_lval, this_type),
                               rhs->EmitIr(ctx).get<ir::FlagsVal>(0)),
                  lhs_lval);
        return ir::Results{};
      }

      auto land_block = ir::CompiledFn::Current->AddBlock();
      auto more_block = ir::CompiledFn::Current->AddBlock();

      auto lhs_val       = lhs->EmitIr(ctx).get<bool>(0);
      auto lhs_end_block = ir::BasicBlock::Current;
      ir::CondJump(lhs_val, more_block, land_block);

      ir::BasicBlock::Current = more_block;
      auto rhs_val            = rhs->EmitIr(ctx).get<bool>(0);
      auto rhs_end_block      = ir::BasicBlock::Current;
      ir::UncondJump(land_block);

      ir::BasicBlock::Current = land_block;

      return ir::Results{ir::MakePhi<bool>(
          ir::Phi(type::Bool),
          {{lhs_end_block, rhs_val}, {rhs_end_block, false}})};
    } break;
    case frontend::Operator::AddEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIr(ctx);
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Add(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::SubEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIr(ctx);
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Sub(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::DivEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIr(ctx);
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Div(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::ModEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIr(ctx);
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t>(rhs_type, [&](auto type_holder) {
        using T = typename decltype(type_holder)::type;
        ir::Store(ir::Div(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)), lhs_lval);
      });
      return ir::Results{};
    } break;
    case frontend::Operator::MulEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIr(ctx);
      type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, float, double>(
          rhs_type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Mul(ir::Load<T>(lhs_lval), rhs_ir.get<T>(0)),
                      lhs_lval);
          });
      return ir::Results{};
    } break;
    case frontend::Operator::XorEq: {
      if (lhs_type == type::Bool) {
        auto lhs_lval = lhs->EmitLVal(ctx)[0];
        auto rhs_ir   = rhs->EmitIr(ctx).get<bool>(0);
        ir::Store(ir::XorBool(ir::Load<bool>(lhs_lval), rhs_ir), lhs_lval);
      } else if (lhs_type->is<type::Flags>()) {
        auto *flags_type = &lhs_type->as<type::Flags>();
        auto lhs_lval    = lhs->EmitLVal(ctx)[0];
        auto rhs_ir      = rhs->EmitIr(ctx).get<ir::FlagsVal>(0);
        ir::Store(
            ir::XorFlags(flags_type,
                         ir::Load<ir::FlagsVal>(lhs_lval, flags_type), rhs_ir),
            lhs_lval);
      } else {
        UNREACHABLE(lhs_type);
      }
      return ir::Results{};
    } break;
    default: UNREACHABLE(*this);
  }
  UNREACHABLE(*this);
}

}  // namespace ast
