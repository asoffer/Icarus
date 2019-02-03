#include "ast/binop.h"

#include "ast/comma_list.h"
#include "ast/fn_args.h"
#include "ast/overload_set.h"
#include "backend/eval.h"
#include "base/check.h"
#include "context.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "type/array.h"
#include "type/cast.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace ir {
RegisterOr<type::Type const *> Tup(base::vector<Val> const &entries);
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
using base::check::Is;
using base::check::Not;

std::string Binop::to_string(size_t n) const {
  std::stringstream ss;
  if (op == Language::Operator::Index) {
    ss << lhs->to_string(n) << "[" << rhs->to_string(n) << "]";
    return ss.str();
  }

  ss << "(" << lhs->to_string(n) << ")";
  switch (op) {
    case Language::Operator::Arrow: ss << " -> "; break;
    case Language::Operator::Add: ss << " + "; break;
    case Language::Operator::Sub: ss << " - "; break;
    case Language::Operator::Mul: ss << " * "; break;
    case Language::Operator::Div: ss << " / "; break;
    case Language::Operator::Mod: ss << " % "; break;
    case Language::Operator::Assign: ss << " = "; break;
    case Language::Operator::OrEq: ss << " |= "; break;
    case Language::Operator::XorEq: ss << " ^= "; break;
    case Language::Operator::AndEq: ss << " &= "; break;
    case Language::Operator::AddEq: ss << " += "; break;
    case Language::Operator::SubEq: ss << " -= "; break;
    case Language::Operator::MulEq: ss << " *= "; break;
    case Language::Operator::DivEq: ss << " /= "; break;
    case Language::Operator::ModEq: ss << " %= "; break;
    case Language::Operator::As: ss << " as "; break;
    case Language::Operator::When: ss << " when "; break;
    default: UNREACHABLE();
  }
  ss << "(" << rhs->to_string(n) << ")";

  return ss.str();
}

void Binop::assign_scope(Scope *scope) {
  scope_ = scope;
  lhs->assign_scope(scope);
  rhs->assign_scope(scope);
}

VerifyResult Binop::VerifyType(Context *ctx) {
  auto lhs_result = lhs->VerifyType(ctx);
  auto rhs_result = rhs->VerifyType(ctx);
  if (!lhs_result.ok() || !rhs_result.ok()) { return VerifyResult::Error(); }

  using Language::Operator;
  switch (op) {
    case Operator::Assign: {
      // TODO if lhs is reserved?
      if (!type::VerifyAssignment(span, lhs_result.type_, rhs_result.type_,
                                  ctx)) {
        return VerifyResult::Error();
      }
      return VerifyResult::NonConstant(type::Void());
    } break;
    case Operator::Index: {
      if (lhs_result.type_ == type::ByteView) {
        if (rhs_result.type_ != type::Int32) {  // TODO other sizes
          ctx->error_log_.InvalidByteViewIndex(span, rhs_result.type_);
        }
        return VerifyResult(ctx->set_type(this, type::Nat8),
                            rhs_result.const_);  // TODO is nat8 what I want?
      } else if (auto *lhs_array_type =
                     lhs_result.type_->if_as<type::Array>()) {
        auto *t = ctx->set_type(this, lhs_array_type->data_type);
        if (rhs_result.type_ != type::Int32) {  // TODO other sizes
          ctx->error_log_.NonIntegralArrayIndex(span, rhs_result.type_);
        }
        return VerifyResult(t, rhs_result.const_);
      } else if (auto *lhs_buf_type =
                     lhs_result.type_->if_as<type::BufferPointer>()) {
        auto *t = ctx->set_type(this, lhs_buf_type->pointee);
        if (rhs_result.type_ != type::Int32) {  // TODO other sizes
          ctx->error_log_.NonIntegralArrayIndex(span, rhs_result.type_);
        }
        return VerifyResult(t, rhs_result.const_);
      } else {
        ctx->error_log_.InvalidIndexing(span, lhs_result.type_);
        return VerifyResult::Error();
      }
    } break;
    case Operator::As: {
      if (rhs_result.type_ != type::Type_) {
        ctx->error_log_.CastToNonType(span);
        return VerifyResult::Error();
      }
      if (!rhs_result.const_) {
        ctx->error_log_.CastToNonConstantType(span);
        return VerifyResult::Error();
      }
      auto *t = ctx->set_type(
          this, ASSERT_NOT_NULL(
                    backend::EvaluateAs<type::Type const *>(rhs.get(), ctx)));
      if (t->is<type::Struct>()) {
        FnArgs<Expression *> args;
        args.pos_ = base::vector<Expression *>{{lhs.get()}};
        OverloadSet os(scope_, "as", ctx);
        os.add_adl("as", t);
        os.add_adl("as", lhs_result.type_);
        os.keep_return(t);

        auto *ret_type = DispatchTable::MakeOrLogError(this, args, os, ctx);
        if (ret_type == nullptr) { return VerifyResult::Error(); }
        ASSERT(t == ret_type);
        return VerifyResult(ret_type, lhs_result.const_);

      } else {
        if (!type::CanCast(lhs_result.type_, t)) {
          LOG << this;
          NOT_YET("log an error", lhs_result.type_, t);
        }
        return VerifyResult(t, lhs_result.const_);
      }
    }
    case Operator::XorEq:
      if (lhs_result.type_ == rhs_result.type_ &&
          (lhs_result.type_ == type::Bool ||
           lhs_result.type_->is<type::Flags>())) {
        ctx->set_type(this, lhs_result.type_);
        return lhs_result;
      } else {
        ctx->error_log_.XorEqNeedsBoolOrFlags(span);
        return VerifyResult::Error();
      }
    case Operator::AndEq:
      if (lhs_result.type_ == rhs_result.type_ &&
          (lhs_result.type_ == type::Bool ||
           lhs_result.type_->is<type::Flags>())) {
        ctx->set_type(this, lhs_result.type_);
        return lhs_result;
      } else {
        ctx->error_log_.AndEqNeedsBoolOrFlags(span);
        return VerifyResult::Error();
      }
    case Operator::OrEq:
      if (lhs_result.type_ == rhs_result.type_ &&
          (lhs_result.type_ == type::Bool ||
           lhs_result.type_->is<type::Flags>())) {
        ctx->set_type(this, lhs_result.type_);
        return lhs_result;
      } else {
        ctx->error_log_.OrEqNeedsBoolOrFlags(span);
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
      args.pos_ = base::vector<Expression *>{{lhs.get(), rhs.get()}};          \
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
        args.pos_ = base::vector<Expression *>{{lhs.get(), rhs.get()}};
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
        args.pos_ = base::vector<Expression *>{{lhs.get(), rhs.get()}};
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
        ctx->error_log_.NonTypeFunctionInput(span);
      }

      if (!IsTypeOrTupleOfTypes(rhs_result.type_)) {
        t = nullptr;
        ctx->error_log_.NonTypeFunctionOutput(span);
      }

      if (t == nullptr) { return VerifyResult::Error(); }

      return VerifyResult(ctx->set_type(this, type::Type_),
                          lhs_result.const_ && rhs_result.const_);
    }
    default: UNREACHABLE();
  }
  UNREACHABLE(static_cast<int>(op));
}

void Binop::Validate(Context *ctx) {
  lhs->Validate(ctx);
  rhs->Validate(ctx);
}

void Binop::ExtractJumps(JumpExprs *rets) const {
  lhs->ExtractJumps(rets);
  rhs->ExtractJumps(rets);
}

base::vector<ir::Val> Binop::EmitIR(Context *ctx) {
  auto *lhs_type = ctx->type_of(lhs.get());
  auto *rhs_type = ctx->type_of(rhs.get());

  if (auto *dispatch_table = ctx->dispatch_table(this)) {
    // TODO struct is not exactly right. we really mean user-defined
    FnArgs<std::pair<Expression *, base::vector<ir::Val>>> args;
    args.pos_.reserve(2);
    args.pos_.emplace_back(lhs.get(), lhs->EmitIR(ctx));
    args.pos_.emplace_back(rhs.get(), rhs->EmitIR(ctx));

    return dispatch_table->EmitCall(args, ASSERT_NOT_NULL(ctx->type_of(this)),
                                    ctx);
  }

  switch (op) {
    case Language::Operator::Add: {
      auto lhs_ir = lhs->EmitIR(ctx)[0];
      auto rhs_ir = rhs->EmitIR(ctx)[0];
      return {
          type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64, float, double>(
              rhs_ir.type, [&](auto type_holder) {
                using T = typename decltype(type_holder)::type;
                return ir::ValFrom(
                    ir::Add(lhs_ir.reg_or<T>(), rhs_ir.reg_or<T>()));
              })};
    } break;
    case Language::Operator::Sub: {
      auto lhs_ir = lhs->EmitIR(ctx)[0];
      auto rhs_ir = rhs->EmitIR(ctx)[0];
      return {
          type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64, float, double>(
              rhs_ir.type, [&](auto type_holder) {
                using T = typename decltype(type_holder)::type;
                return ir::ValFrom(
                    ir::Sub(lhs_ir.reg_or<T>(), rhs_ir.reg_or<T>()));
              })};
    } break;
    case Language::Operator::Mul: {
      auto lhs_ir = lhs->EmitIR(ctx)[0];
      auto rhs_ir = rhs->EmitIR(ctx)[0];
      return {
          type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64, float, double>(
              rhs_ir.type, [&](auto type_holder) {
                using T = typename decltype(type_holder)::type;
                return ir::ValFrom(
                    ir::Mul(lhs_ir.reg_or<T>(), rhs_ir.reg_or<T>()));
              })};
    } break;
    case Language::Operator::Div: {
      auto lhs_ir = lhs->EmitIR(ctx)[0];
      auto rhs_ir = rhs->EmitIR(ctx)[0];
      return {
          type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64, float, double>(
              rhs_ir.type, [&](auto type_holder) {
                using T = typename decltype(type_holder)::type;
                return ir::ValFrom(
                    ir::Div(lhs_ir.reg_or<T>(), rhs_ir.reg_or<T>()));
              })};
    } break;
    case Language::Operator::Mod: {
      auto lhs_ir = lhs->EmitIR(ctx)[0];
      auto rhs_ir = rhs->EmitIR(ctx)[0];
      return {type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64>(
          rhs_ir.type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::ValFrom(ir::Mod(lhs_ir.reg_or<T>(), rhs_ir.reg_or<T>()));
          })};
    } break;
    case Language::Operator::As: {
      auto *this_type  = ASSERT_NOT_NULL(ctx->type_of(this));
      auto vals        = lhs->EmitIR(ctx);
      if (this_type == type::Type_) {
        base::vector<type::Type const *> entries;
        entries.reserve(vals.size());
        for (auto const& val : vals) {
          // TODO what about incomplete structs?
          entries.push_back(std::get<type::Type const*>(val.value));
        }
        return {ir::Val(type::Tup(entries))};
      }
      return {ir::Cast(vals[0].type, this_type, vals[0])};
    } break;
    case Language::Operator::Arrow: {
      // TODO ugly hack.
      std::vector<ir::Val> lhs_vals, rhs_vals;
      if (auto *l = lhs->if_as<CommaList>()) {
        for (auto &e : l->exprs_) { lhs_vals.push_back(e->EmitIR(ctx)[0]); }
      } else {
        lhs_vals.push_back(lhs->EmitIR(ctx)[0]);
      }
      if (auto *r = rhs->if_as<CommaList>()) {
        for (auto &e : r->exprs_) { rhs_vals.push_back(e->EmitIR(ctx)[0]); }
      } else {
        rhs_vals.push_back(rhs->EmitIR(ctx)[0]);
      }

      auto reg_or_type = ir::Arrow(ir::Tup(lhs_vals), ir::Tup(rhs_vals));
      return {ir::ValFrom(reg_or_type)};
    } break;
    case Language::Operator::Assign: {
      // TODO support splatting.
      auto lhs_lvals = lhs->EmitLVal(ctx);
      if (lhs_lvals.size() != 1) { NOT_YET(); }

      auto rhs_vals = rhs->EmitIR(ctx);
      if (rhs_vals.size() != 1) { NOT_YET(); }

      lhs_type->EmitMoveAssign(rhs_type, rhs_vals[0], lhs_lvals[0], ctx);

      return {};
    } break;
    case Language::Operator::OrEq: {
      auto *this_type = ctx->type_of(this);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = lhs->EmitLVal(ctx)[0];
        ir::Store(ir::OrFlags(&this_type->as<type::Flags>(),
                              ir::Load<ir::FlagsVal>(lhs_lval, this_type),
                              rhs->EmitIR(ctx)[0].reg_or<ir::FlagsVal>()),
                  lhs_lval);
        return {};
      }
      auto land_block = ir::Func::Current->AddBlock();
      auto more_block = ir::Func::Current->AddBlock();

      auto lhs_val       = lhs->EmitIR(ctx)[0].reg_or<bool>();
      auto lhs_end_block = ir::BasicBlock::Current;
      ir::CondJump(lhs_val, land_block, more_block);

      ir::BasicBlock::Current = more_block;
      auto rhs_val            = rhs->EmitIR(ctx)[0].reg_or<bool>();
      auto rhs_end_block      = ir::BasicBlock::Current;
      ir::UncondJump(land_block);

      ir::BasicBlock::Current = land_block;

      return {ir::ValFrom(ir::MakePhi<bool>(
          ir::Phi(type::Bool),
          {{lhs_end_block, true}, {rhs_end_block, rhs_val}}))};
    } break;
    case Language::Operator::AndEq: {
      auto *this_type = ctx->type_of(this);
      if (this_type->is<type::Flags>()) {
        auto lhs_lval = lhs->EmitLVal(ctx)[0];
        ir::Store(ir::AndFlags(&this_type->as<type::Flags>(),
                               ir::Load<ir::FlagsVal>(lhs_lval, this_type),
                               rhs->EmitIR(ctx)[0].reg_or<ir::FlagsVal>()),
                  lhs_lval);
        return {};
      }

      auto land_block = ir::Func::Current->AddBlock();
      auto more_block = ir::Func::Current->AddBlock();

      auto lhs_val       = lhs->EmitIR(ctx)[0].reg_or<bool>();
      auto lhs_end_block = ir::BasicBlock::Current;
      ir::CondJump(lhs_val, more_block, land_block);

      ir::BasicBlock::Current = more_block;
      auto rhs_val            = rhs->EmitIR(ctx)[0].reg_or<bool>();
      auto rhs_end_block      = ir::BasicBlock::Current;
      ir::UncondJump(land_block);

      ir::BasicBlock::Current = land_block;

      return {ir::ValFrom(ir::MakePhi<bool>(
          ir::Phi(type::Bool),
          {{lhs_end_block, rhs_val}, {rhs_end_block, false}}))};
    } break;
    case Language::Operator::AddEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIR(ctx)[0];
      type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64, float, double>(
          rhs_ir.type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Add(ir::Load<T>(lhs_lval), rhs_ir.reg_or<T>()),
                      lhs_lval);
          });
      return {};
    } break;
    case Language::Operator::SubEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIR(ctx)[0];
      type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64, float, double>(
          rhs_ir.type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Sub(ir::Load<T>(lhs_lval), rhs_ir.reg_or<T>()),
                      lhs_lval);
          });
      return {};
    } break;
    case Language::Operator::DivEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIR(ctx)[0];
      type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64, float, double>(
          rhs_ir.type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Div(ir::Load<T>(lhs_lval), rhs_ir.reg_or<T>()),
                      lhs_lval);
          });
      return {};
    } break;
    case Language::Operator::ModEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIR(ctx)[0];
      type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64>(
          rhs_ir.type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Div(ir::Load<T>(lhs_lval), rhs_ir.reg_or<T>()),
                      lhs_lval);
          });
      return {};
    } break;
    case Language::Operator::MulEq: {
      auto lhs_lval = lhs->EmitLVal(ctx)[0];
      auto rhs_ir   = rhs->EmitIR(ctx)[0];
      type::ApplyTypes<i8, i16, i32, i64, u8, u16, u32, u64, float, double>(
          rhs_ir.type, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            ir::Store(ir::Mul(ir::Load<T>(lhs_lval), rhs_ir.reg_or<T>()),
                      lhs_lval);
          });
      return {};
    } break;
    case Language::Operator::XorEq: {
      if (lhs_type == type::Bool) {
        auto lhs_lval = lhs->EmitLVal(ctx)[0];
        auto rhs_ir   = rhs->EmitIR(ctx)[0].reg_or<bool>();
        ir::Store(ir::XorBool(ir::Load<bool>(lhs_lval), rhs_ir), lhs_lval);
      } else if (lhs_type->is<type::Flags>()) {
        auto *flags_type = &lhs_type->as<type::Flags>();
        auto lhs_lval    = lhs->EmitLVal(ctx)[0];
        auto rhs_ir      = rhs->EmitIR(ctx)[0].reg_or<ir::FlagsVal>();
        ir::Store(ir::XorFlags(flags_type, ir::Load<ir::FlagsVal>(lhs_lval, flags_type),
                               rhs_ir),
                  lhs_lval);
      } else {
        UNREACHABLE(lhs_type);
      }
      return {};
    } break;
    case Language::Operator::Index: {
      auto *this_type = ctx->type_of(this);
      auto lval       = EmitLVal(ctx)[0];
      return {ir::Val::Reg(ir::PtrFix(lval.reg_, this_type), this_type)};
    } break;
    default: UNREACHABLE(*this);
  }
}

base::vector<ir::RegisterOr<ir::Addr>> Binop::EmitLVal(Context *ctx) {
  switch (op) {
    case Language::Operator::As: NOT_YET();
    case Language::Operator::Index: 
      if (auto *t = ctx->type_of(lhs.get()); t->is<type::Array>()) {
        auto lval = lhs->EmitLVal(ctx)[0];
        if (!lval.is_reg_) { NOT_YET(this, ctx->type_of(this)); }
        return {ir::Index(type::Ptr(ctx->type_of(lhs.get())), lval.reg_,
                          rhs->EmitIR(ctx)[0].reg_or<i32>())};
      } else if (t->is<type::BufferPointer>()) {
        return {ir::PtrIncr(std::get<ir::Register>(lhs->EmitIR(ctx)[0].value),
                            rhs->EmitIR(ctx)[0].reg_or<i32>(),
                            type::Ptr(t->as<type::BufferPointer>().pointee))};
      } else if (t == type::ByteView) {
        // TODO interim until you remove string_view and replace it with Addr
        // entirely.
        return {ir::PtrIncr(
            ir::GetString(std::string(
                std::get<std::string_view>(lhs->EmitIR(ctx)[0].value))),
            rhs->EmitIR(ctx)[0].reg_or<i32>(), type::Ptr(type::Nat8))};
      }
      [[fallthrough]];
    default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}

}  // namespace ast
