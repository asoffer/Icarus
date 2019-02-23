#include "ast/binop.h"

#include "ast/comma_list.h"
#include "ast/fn_args.h"
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "ir/components.h"
#include "ir/func.h"
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

namespace {
bool IsTypeOrTupleOfTypes(type::Type const *t) {
  if (t == type::Type_) { return true; }
  if (!t->is<type::Tuple>()) { return false; }
  auto &entries = t->as<type::Tuple>().entries_;
  return std::all_of(entries.begin(), entries.end(),
                     [](type::Type const *ty) { return ty == type::Type_; });
}

}  // namespace

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

bool Binop::InferType(type::Type const *t, InferenceState *state) const {
  // TODO consider the possibility for overloadable operators to be generic
  // struct and therefore not always returning false.
  switch (op) {
    case frontend::Operator::Arrow:
      if (auto *f = t->if_as<type::Function>()) {
        // TODO is tuple right?
        state->match_queue_.emplace(lhs.get(), type::Tup(f->input));
        state->match_queue_.emplace(rhs.get(), type::Tup(f->output));
        return true;
      } else {
        return false;
      }
    case frontend::Operator::Add:
    case frontend::Operator::Sub:
    case frontend::Operator::Mul:
    case frontend::Operator::Div:
    case frontend::Operator::Mod:
    case frontend::Operator::Assign:
    case frontend::Operator::OrEq:
    case frontend::Operator::XorEq:
    case frontend::Operator::AndEq:
    case frontend::Operator::AddEq:
    case frontend::Operator::SubEq:
    case frontend::Operator::MulEq:
    case frontend::Operator::DivEq:
    case frontend::Operator::ModEq:
    case frontend::Operator::When: return false;
    default: UNREACHABLE();
  }
}

void Binop::assign_scope(Scope *scope) {
  scope_ = scope;
  lhs->assign_scope(scope);
  rhs->assign_scope(scope);
}

void Binop::DependentDecls(base::Graph<Declaration *> *g,
                           Declaration *d) const {
  lhs->DependentDecls(g, d);
  rhs->DependentDecls(g, d);
}

VerifyResult Binop::VerifyType(Context *ctx) {
  auto lhs_result = lhs->VerifyType(ctx);
  auto rhs_result = rhs->VerifyType(ctx);
  if (!lhs_result.ok() || !rhs_result.ok()) { return VerifyResult::Error(); }

  using frontend::Operator;
  switch (op) {
    case Operator::Assign: {
      // TODO if lhs is reserved?
      if (!type::VerifyAssignment(span, lhs_result.type_, rhs_result.type_,
                                  ctx)) {
        return VerifyResult::Error();
      }
      return VerifyResult::NonConstant(type::Void());
    } break;
    case Operator::XorEq:
      if (lhs_result.type_ == rhs_result.type_ &&
          (lhs_result.type_ == type::Bool ||
           lhs_result.type_->is<type::Flags>())) {
        ctx->set_type(this, lhs_result.type_);
        return lhs_result;
      } else {
        ctx->error_log()->XorEqNeedsBoolOrFlags(span);
        return VerifyResult::Error();
      }
    case Operator::AndEq:
      if (lhs_result.type_ == rhs_result.type_ &&
          (lhs_result.type_ == type::Bool ||
           lhs_result.type_->is<type::Flags>())) {
        ctx->set_type(this, lhs_result.type_);
        return lhs_result;
      } else {
        ctx->error_log()->AndEqNeedsBoolOrFlags(span);
        return VerifyResult::Error();
      }
    case Operator::OrEq:
      if (lhs_result.type_ == rhs_result.type_ &&
          (lhs_result.type_ == type::Bool ||
           lhs_result.type_->is<type::Flags>())) {
        ctx->set_type(this, lhs_result.type_);
        return lhs_result;
      } else {
        ctx->error_log()->OrEqNeedsBoolOrFlags(span);
        return VerifyResult::Error();
      }

#define CASE(OpName, symbol, return_type)                                      \
  case Operator::OpName: {                                                     \
    bool is_const = lhs_result.const_ && rhs_result.const_;                    \
    if (type::IsNumeric(lhs_result.type_) &&                                   \
        type::IsNumeric(rhs_result.type_)) {                                   \
      if (lhs_result.type_ == rhs_result.type_) {                              \
        return VerifyResult(ctx->set_type(this, (return_type)), is_const);     \
      } else {                                                                 \
        NOT_YET("Log an error");                                               \
        return VerifyResult::Error();                                          \
      }                                                                        \
    } else {                                                                   \
      FnArgs<Expression *> args;                                               \
      args.pos_ = std::vector<Expression *>{{lhs.get(), rhs.get()}};           \
      OverloadSet os(scope_, symbol, ctx);                                     \
      os.add_adl(symbol, lhs_result.type_);                                    \
      os.add_adl(symbol, rhs_result.type_);                                    \
                                                                               \
      auto *ret_type = DispatchTable::MakeOrLogError(this, args, os, ctx);     \
      if (ret_type == nullptr) { return VerifyResult::Error(); }               \
      if (ret_type->is<type::Tuple>()) { NOT_YET(); }                          \
      return VerifyResult(ctx->set_type(this, ret_type), lhs_result.const_);   \
    }                                                                          \
  } break;
      CASE(Sub, "-", lhs_result.type_);
      CASE(Mul, "-", lhs_result.type_);
      CASE(Div, "/", lhs_result.type_);
      CASE(Mod, "%", lhs_result.type_);
      CASE(SubEq, "-=", type::Void());
      CASE(MulEq, "*=", type::Void());
      CASE(DivEq, "/=", type::Void());
      CASE(ModEq, "%=", type::Void());
#undef CASE
    case Operator::Add: {
      bool is_const = lhs_result.const_ && rhs_result.const_;
      if (type::IsNumeric(lhs_result.type_) &&
          type::IsNumeric(rhs_result.type_)) {
        if (lhs_result.type_ == rhs_result.type_) {
          return VerifyResult(ctx->set_type(this, lhs_result.type_), is_const);
        } else {
          NOT_YET("Log an error");
          return VerifyResult::Error();
        }
      } else {
        FnArgs<Expression *> args;
        args.pos_ = std::vector<Expression *>{{lhs.get(), rhs.get()}};
        OverloadSet os(scope_, "+", ctx);
        os.add_adl("+", lhs_result.type_);
        os.add_adl("+", rhs_result.type_);

        auto *ret_type = DispatchTable::MakeOrLogError(this, args, os, ctx);
        if (ret_type == nullptr) { return VerifyResult::Error(); }
        if (ret_type->is<type::Tuple>()) { NOT_YET(); }
        return VerifyResult(ctx->set_type(this, ret_type), is_const);
      }
    } break;
    case Operator::AddEq: {
      bool is_const = lhs_result.const_ && rhs_result.const_;
      if (type::IsNumeric(lhs_result.type_) &&
          type::IsNumeric(rhs_result.type_)) {
        if (lhs_result.type_ == rhs_result.type_) {
          return VerifyResult(ctx->set_type(this, type::Void()), is_const);
        } else {
          NOT_YET("Log an error");
          return VerifyResult::Error();
        }
      } else {
        FnArgs<Expression *> args;
        args.pos_ = std::vector<Expression *>{{lhs.get(), rhs.get()}};
        OverloadSet os(scope_, "+=", ctx);
        os.add_adl("+=", lhs_result.type_);
        os.add_adl("+=", rhs_result.type_);

        auto *ret_type = DispatchTable::MakeOrLogError(this, args, os, ctx);
        if (ret_type == nullptr) { return VerifyResult::Error(); }
        if (ret_type->is<type::Tuple>()) { NOT_YET(); }
        return VerifyResult(ctx->set_type(this, ret_type), is_const);
      }
    } break;
    case Operator::Arrow: {
      type::Type const *t = type::Type_;
      if (!IsTypeOrTupleOfTypes(lhs_result.type_)) {
        t = nullptr;
        ctx->error_log()->NonTypeFunctionInput(span);
      }

      if (!IsTypeOrTupleOfTypes(rhs_result.type_)) {
        t = nullptr;
        ctx->error_log()->NonTypeFunctionOutput(span);
      }

      if (t == nullptr) { return VerifyResult::Error(); }

      return VerifyResult(ctx->set_type(this, type::Type_),
                          lhs_result.const_ && rhs_result.const_);
    }
    default: UNREACHABLE();
  }
  UNREACHABLE(static_cast<int>(op));
}

void Binop::ExtractJumps(JumpExprs *rets) const {
  lhs->ExtractJumps(rets);
  rhs->ExtractJumps(rets);
}

ir::Results Binop::EmitIr(Context *ctx) {
  auto *lhs_type = ctx->type_of(lhs.get());
  auto *rhs_type = ctx->type_of(rhs.get());

  if (auto *dispatch_table = ctx->dispatch_table(this)) {
    // TODO struct is not exactly right. we really mean user-defined
    FnArgs<std::pair<Expression *, ir::Results>> args;
    args.pos_.reserve(2);
    args.pos_.emplace_back(lhs.get(), lhs->EmitIr(ctx));
    args.pos_.emplace_back(rhs.get(), rhs->EmitIr(ctx));

    return dispatch_table->EmitCall(args, ASSERT_NOT_NULL(ctx->type_of(this)),
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
      auto land_block = ir::Func::Current->AddBlock();
      auto more_block = ir::Func::Current->AddBlock();

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

      auto land_block = ir::Func::Current->AddBlock();
      auto more_block = ir::Func::Current->AddBlock();

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
