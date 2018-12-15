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
#include "type/char_buffer.h"
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

void ForEachExpr(ast::Expression *expr,
                 std::function<void(ast::Expression *)> const &fn) {
  if (expr->is<ast::CommaList>()) {
    auto const &exprs = expr->as<ast::CommaList>().exprs_;
    for (size_t i = 0; i < exprs.size(); ++i) { fn(exprs[i].get()); }
  } else {
    fn(expr);
  }
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
    case Language::Operator::Assign: ss << " <<:=>> "; break;
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

type::Type const *Binop::VerifyType(Context *ctx) {
  auto *lhs_type = lhs->VerifyType(ctx);
  auto *rhs_type = rhs->VerifyType(ctx);
  if (lhs_type == nullptr || rhs_type == nullptr) { return nullptr; }

  using Language::Operator;
  // TODO if lhs is reserved?
  if (op == Operator::Assign) {
    if (auto *lhs_tup = lhs_type->if_as<type::Tuple>()) {
      if (auto *rhs_tup = rhs_type->if_as<type::Tuple>()) {
        if (lhs_tup->entries_.size() != rhs_tup->entries_.size()) {
          ctx->error_log_.MismatchedAssignmentSize(
              span, lhs_tup->entries_.size(), rhs_tup->entries_.size());
          return nullptr;
        }

        for (size_t i = 0; i < lhs_tup->entries_.size(); ++i) {
          if (!type::CanCastImplicitly(rhs_tup->entries_[i],
                                       lhs_tup->entries_[i])) {
            NOT_YET("log an error");
          }
        }
      } else {
        // TODO should you allow this for struct user-defined types?
        // z: complex
        // z = (3, 4) // Interpretted as (=)(&z, 3, 4)?
        ctx->error_log_.MismatchedAssignmentSize(span, lhs_tup->entries_.size(),
                                                 1);
        return nullptr;
      }
    } else {
      if (auto*rhs_tup = rhs_type->if_as<type::Tuple>()) {
        ctx->error_log_.MismatchedAssignmentSize(span, 1,
                                                 rhs_tup->entries_.size());
      } else {
        if (!type::CanCastImplicitly(rhs_type, lhs_type)) {
          NOT_YET("log an error");
        }
      }
    }

    return type::Void();
  }

  switch (op) {
    case Operator::Index: {
      if (lhs_type->is<type::CharBuffer>()) {
        if (rhs_type != type::Int32) { // TODO other sizes
          ctx->error_log_.InvalidCharBufIndex(span, rhs_type);
        }
        return ctx->set_type(this, type::Char);
      } else if (auto *lhs_array_type = lhs_type->if_as<type::Array>()) {
        auto *t = ctx->set_type(this, lhs_array_type->data_type);
        if (rhs_type == type::Int32) {  // TODO other sizes
          ctx->error_log_.NonIntegralArrayIndex(span, rhs_type);
        }
        return t;
      } else if (auto *lhs_buf_type = lhs_type->if_as<type::BufferPointer>()) {
        auto *t =
            ctx->set_type(this, lhs_buf_type->pointee);
        if (rhs_type != type::Int32) {  // TODO other sizes
          ctx->error_log_.NonIntegralArrayIndex(span, rhs_type);
        }
        return t;
      } else {
        ctx->error_log_.InvalidIndexing(span, lhs_type);
        return nullptr;
      }
    } break;
    case Operator::As: {
      auto *t = backend::EvaluateAs<type::Type const *>(rhs.get(), ctx);
      ctx->set_type(this, t);
      if (t->is<type::Struct>()) {
        FnArgs<Expression *> args;
        args.pos_           = base::vector<Expression *>{{lhs.get()}};
        type::Type const *ret_type = nullptr;
        OverloadSet overload_set(scope_, "as", ctx);
        overload_set.keep_return(t);

        std::tie(dispatch_table_, t) =
            DispatchTable::Make(args, overload_set, ctx);
        ASSERT(t, Not(Is<type::Tuple>()));
        if (t == nullptr) {
          ctx->error_log_.NoMatchingOperator("as", lhs_type, rhs_type, span);
          return nullptr;
        } else {
          return ctx->set_type(this, t);
        }
      } else {
        if (!type::CanCast(lhs_type, t)) {
          LOG << this;
          NOT_YET("log an error", lhs_type, t);
        }
        return t;
      }
    }
    case Operator::XorEq:
      if (lhs_type == rhs_type &&
          (lhs_type == type::Bool || lhs_type->is<type::Flags>())) {
        return ctx->set_type(this, lhs_type);
      } else {
        ctx->error_log_.XorEqNeedsBoolOrFlags(span);
        return nullptr;
      }
    case Operator::AndEq:
      if (lhs_type == rhs_type &&
          (lhs_type == type::Bool || lhs_type->is<type::Flags>())) {
        return ctx->set_type(this, lhs_type);
      } else {
        ctx->error_log_.AndEqNeedsBoolOrFlags(span);
        return nullptr;
      }
    case Operator::OrEq:
      if (lhs_type == rhs_type &&
          (lhs_type == type::Bool || lhs_type->is<type::Flags>())) {
        return ctx->set_type(this, lhs_type);
      } else {
        ctx->error_log_.OrEqNeedsBoolOrFlags(span);
        return nullptr;
      }

#define CASE(OpName, symbol, ret_type)                                          \
  case Operator::OpName: {                                                      \
    if (type::IsNumeric(lhs_type) && type::IsNumeric(rhs_type)) {               \
      if (lhs_type == rhs_type) {                                               \
        auto *t = (ret_type);                                                   \
        return ctx->set_type(this, t);                                          \
      } else {                                                                  \
        ctx->error_log_.NoMatchingOperator(symbol, lhs_type, rhs_type, span);   \
        return nullptr;                                                         \
      }                                                                         \
    } else {                                                                    \
      FnArgs<Expression *> args;                                                \
      args.pos_           = base::vector<Expression *>{{lhs.get(), rhs.get()}}; \
      type::Type const *t = nullptr;                                            \
      std::tie(dispatch_table_, t) =                                            \
          DispatchTable::Make(args, OverloadSet(scope_, symbol, ctx), ctx);     \
      if (t == nullptr) {                                                       \
        ctx->error_log_.NoMatchingOperator(symbol, lhs_type, rhs_type, span);   \
        return nullptr;                                                         \
      } else {                                                                  \
        ASSERT(t, Not(Is<type::Tuple>()));                                      \
        return ctx->set_type(this, t);                                          \
      }                                                                         \
    }                                                                           \
  } break;

      CASE(Sub, "-", lhs_type);
      CASE(Div, "/", lhs_type);
      CASE(Mod, "%", lhs_type);
      CASE(SubEq, "-=", type::Void());
      CASE(MulEq, "*=", type::Void());
      CASE(DivEq, "/=", type::Void());
      CASE(ModEq, "%=", type::Void());
#undef CASE
    case Operator::Add: {
      if (type::IsNumeric(lhs_type) && type::IsNumeric(rhs_type)) {
        if (lhs_type == rhs_type) {
          return ctx->set_type(this, lhs_type);
        } else {
          ctx->error_log_.NoMatchingOperator("+", lhs_type, rhs_type, span);
          return nullptr;
        }
      } else if (lhs_type->is<type::CharBuffer>() &&
                 rhs_type->is<type::CharBuffer>()) {
        auto *t = type::CharBuf(lhs_type->as<type::CharBuffer>().length_ +
                                rhs_type->as<type::CharBuffer>().length_);
        return ctx->set_type(this, t);
      } else {
        FnArgs<Expression *> args;
        args.pos_ = base::vector<Expression *>{{lhs.get(), rhs.get()}};
        type::Type const *t = nullptr;
        std::tie(dispatch_table_, t) =
            DispatchTable::Make(args, OverloadSet(scope_, "+", ctx), ctx);
        ASSERT(t, Not(Is<type::Tuple>()));
        if (t == nullptr) {
          ctx->error_log_.NoMatchingOperator("+", lhs_type, rhs_type, span);
        } else {
          return ctx->set_type(this, t);
        }
        return t;
      }
    } break;
    case Operator::AddEq: {
      if (type::IsNumeric(lhs_type) && type::IsNumeric(rhs_type)) {
        if (lhs_type == rhs_type) {
          return ctx->set_type(this, type::Void());
        } else {
          ctx->error_log_.NoMatchingOperator("+=", lhs_type, rhs_type, span);
          return nullptr;
        }
      } else {
        FnArgs<Expression *> args;
        args.pos_ = base::vector<Expression *>{{lhs.get(), rhs.get()}};
        type::Type const *t = nullptr;
        std::tie(dispatch_table_, t) =
            DispatchTable::Make(args, OverloadSet(scope_, "+=", ctx), ctx);
        ASSERT(t, Not(Is<type::Tuple>()));
      }
    } break;
    // Mul is done separately because of the function composition
    case Operator::Mul:
      if (type::IsNumeric(lhs_type) && type::IsNumeric(rhs_type)) {
        if (lhs_type == rhs_type) {
          return ctx->set_type(this, lhs_type);
        } else {
          ctx->error_log_.NoMatchingOperator("*", lhs_type, rhs_type, span);
          return nullptr;
        }
      } else if (lhs_type->is<type::Function>() &&
                 rhs_type->is<type::Function>()) {
        auto *lhs_fn = &lhs_type->as<type::Function>();
        auto *rhs_fn = &rhs_type->as<type::Function>();
        if (rhs_fn->output == lhs_fn->input) {
          auto *t = type::Func({rhs_fn->input}, {lhs_fn->output});
          return ctx->set_type(this, t);
        } else {
          ctx->error_log_.NonComposableFunctions(span);
          return nullptr;
        }
      } else {
        FnArgs<Expression *> args;
        args.pos_ = base::vector<Expression *>{{lhs.get(), rhs.get()}};
        type::Type const *t = nullptr;
        std::tie(dispatch_table_, t) =
            DispatchTable::Make(args, OverloadSet(scope_, "*", ctx), ctx);
        ASSERT(t, Not(Is<type::Tuple>()));
        if (t == nullptr) {
          ctx->error_log_.NoMatchingOperator("+", lhs_type, rhs_type, span);
          return nullptr;
        } else {
          return ctx->set_type(this, t);
        }
      }
    case Operator::Arrow: {
      type::Type const *t = type::Type_;
      if (!IsTypeOrTupleOfTypes(lhs_type)) {
        t = nullptr;
        ctx->error_log_.NonTypeFunctionInput(span);
      }

      if (!IsTypeOrTupleOfTypes(rhs_type)) {
        t = nullptr;
        ctx->error_log_.NonTypeFunctionOutput(span);
      }

      if (t != nullptr) { ctx->set_type(this, type::Type_); }
      return t;
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

base::vector<ir::Val> ast::Binop::EmitIR(Context *ctx) {
  auto *lhs_type = ctx->type_of(lhs.get());
  auto *rhs_type = ctx->type_of(rhs.get());
  if (dispatch_table_.total_size_ != 0) {
    // TODO struct is not exactly right. we really mean user-defined
    ast::FnArgs<std::pair<ast::Expression *, ir::Val>> args;
    args.pos_.reserve(2);
    args.pos_.emplace_back(lhs.get(), lhs->EmitIR(ctx)[0]);
    args.pos_.emplace_back(rhs.get(), rhs->EmitIR(ctx)[0]);

    return dispatch_table_.EmitCall(args, ASSERT_NOT_NULL(ctx->type_of(this)),
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
      auto *this_type = ctx->type_of(this);
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
      auto reg_or_type =
          ir::Arrow(ir::Tup(lhs->EmitIR(ctx)), ir::Tup(rhs->EmitIR(ctx)));
      return {ir::ValFrom(reg_or_type)};
    } break;
    case Language::Operator::Assign: {
      base::vector<type::Type const *> lhs_types, rhs_types;
      ForEachExpr(rhs.get(), [&ctx, &rhs_types](Expression *expr) {
        auto *expr_type = ctx->type_of(expr);
        if (expr_type->is<type::Tuple>()) {
          rhs_types.insert(rhs_types.end(),
                           expr_type->as<type::Tuple>().entries_.begin(),
                           expr_type->as<type::Tuple>().entries_.end());
        } else {
          rhs_types.push_back(expr_type);
        }
      });
      auto rhs_vals = rhs->EmitIR(ctx);

      // TODO types can be retrieved from the values?
      ForEachExpr(lhs.get(), [&ctx, &lhs_types](ast::Expression *expr) {
        lhs_types.push_back(ctx->type_of(expr));
      });
      auto lhs_lvals = lhs->EmitLVal(ctx);

      ASSERT(lhs_lvals.size() == rhs_vals.size());
      ASSERT(lhs_lvals.size() == lhs_types.size());
      ASSERT(lhs_lvals.size() == rhs_types.size());
      for (size_t i = 0; i < lhs_lvals.size(); ++i) {
        lhs_types[i]->EmitAssign(rhs_types[i], rhs_vals[i], lhs_lvals[i], ctx);
      }
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
      return {ir::Val::Reg(ir::PtrFix(EmitLVal(ctx)[0], this_type), this_type)};
    } break;
    default: UNREACHABLE(*this);
  }
}

base::vector<ir::Register> ast::Binop::EmitLVal(Context *ctx) {
  switch (op) {
    case Language::Operator::As: NOT_YET();
    case Language::Operator::Index: 
      if (auto *t = ctx->type_of(lhs.get()); t->is<type::Array>()) {
        return {ir::Index(type::Ptr(ctx->type_of(this)), lhs->EmitLVal(ctx)[0],
                          rhs->EmitIR(ctx)[0].reg_or<i32>())};
      } else if (t->is<type::BufferPointer>()) {
        return {PtrIncr(std::get<ir::Register>(lhs->EmitIR(ctx)[0].value),
                        rhs->EmitIR(ctx)[0].reg_or<i32>(),
                        type::Ptr(t->as<type::BufferPointer>().pointee))};
      }
      [[fallthrough]];
    default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}

}  // namespace ast
