#include "ast/unop.h"

#include "ast/fn_args.h"
#include "ast/overload_set.h"
#include "ast/terminal.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "base/check.h"
#include "context.h"
#include "ir/func.h"
#include "type/all.h"

base::vector<ir::Val> EmitCallDispatch(
    const ast::FnArgs<std::pair<ast::Expression *, ir::Val>> &args,
    const ast::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx);

void ForEachExpr(ast::Expression *expr,
                 const std::function<void(size_t, ast::Expression *)> &fn);

namespace ast {
using base::check::Is;
using base::check::Not;

std::string Unop::to_string(size_t n) const {
  if (op == Language::Operator::TypeOf) {
    return "(" + operand->to_string(n) + "):?";
  }

  std::stringstream ss;
  switch (op) {
    case Language::Operator::Which: ss << "which "; break;
    case Language::Operator::Mul: ss << "*"; break;
    case Language::Operator::And: ss << "&"; break;
    case Language::Operator::Sub: ss << "-"; break;
    case Language::Operator::Not: ss << "!"; break;
    case Language::Operator::At: ss << "@"; break;
    case Language::Operator::Eval: ss << "$"; break;
    case Language::Operator::Ref: ss << "\\"; break;
    case Language::Operator::Needs: ss << "needs "; break;
    case Language::Operator::Ensure: ss << "ensure "; break;
    case Language::Operator::Pass: break;
    default: { UNREACHABLE(); }
  }

  ss << operand->to_string(n);
  return ss.str();
}

void Unop::assign_scope(Scope *scope) {
  scope_ = scope;
  operand->assign_scope(scope);
}

void Unop::Validate(Context *ctx) { operand->Validate(ctx); }

void Unop::ExtractJumps(JumpExprs *rets) const {
  operand->ExtractJumps(rets);
}

type::Type const *Unop::VerifyType(Context *ctx) {
  VERIFY_OR_RETURN(operand_type, operand);

  switch (op) {
    case Language::Operator::TypeOf:
      ctx->set_type(this, type::Type_);
      return type::Type_;
    case Language::Operator::Eval:
      ctx->set_type(this, operand_type);
      return operand_type;
    case Language::Operator::Which:
      if (!operand_type->is<type::Variant>()) {
        ctx->error_log_.WhichNonVariant(operand_type, span);
      }
      ctx->set_type(this, type::Type_);
      return type::Type_;
    case Language::Operator::At:
      if (operand_type->is<type::Pointer>()) {
        auto *t = operand_type->as<type::Pointer>().pointee;
        ctx->set_type(this, t);
        return t;
      } else {
        ctx->error_log_.DereferencingNonPointer(operand_type, span);
        return nullptr;
      }
    case Language::Operator::And: {
      auto *t = type::Ptr(operand_type);
      ctx->set_type(this, t);
      return t;
    }
    case Language::Operator::Mul:
      if (operand_type != type::Type_) {
        NOT_YET("log an error");
        return nullptr;
      } else {
        ctx->set_type(this, type::Type_);
        return type::Type_;
      }
    case Language::Operator::Sub:
      if (type::IsNumeric(operand_type)) {
        ctx->set_type(this, operand_type);
        return operand_type;
      } else if (operand_type->is<type::Struct>()) {
        FnArgs<Expression *> args;
        args.pos_           = base::vector<Expression *>{operand.get()};
        type::Type const *t = nullptr;
        std::tie(dispatch_table_, t) =
            DispatchTable::Make(args, OverloadSet(scope_, "-", ctx), ctx);
        if (t == nullptr) {
          return nullptr;
        }
        return t;
      }
      NOT_YET();
      return nullptr;
    case Language::Operator::Not:
      if (operand_type == type::Bool) {
        ctx->set_type(this, type::Bool);
        return type::Bool;
      } else if (operand_type->is<type::Flags>()) {
        ctx->set_type(this, operand_type);
        return operand_type;
      } else if (operand_type->is<type::Struct>()) {
        FnArgs<Expression *> args;
        args.pos_           = base::vector<Expression *>{operand.get()};
        type::Type const *t = nullptr;
        std::tie(dispatch_table_, t) =
            DispatchTable::Make(args, OverloadSet(scope_, "!", ctx), ctx);
        ASSERT(t, Not(Is<type::Tuple>()));
        if (t == nullptr) {
          return nullptr;
        }
        return t;
      } else {
        NOT_YET("log an error");
        return nullptr;
      }
    case Language::Operator::Needs:
      ctx->set_type(this, type::Void());
      if (operand_type != type::Bool) {
        ctx->error_log_.PreconditionNeedsBool(operand.get());
      }
      return type::Void();
    case Language::Operator::Ensure:
      ctx->set_type(this, type::Void());
      if (operand_type != type::Bool) {
        ctx->error_log_.PostconditionNeedsBool(operand.get());
      }
      return type::Void();
    case Language::Operator::Pass:
      ctx->set_type(this, operand_type);
      return operand_type;
    default: UNREACHABLE(*this);
  }
}

base::vector<ir::Val> Unop::EmitIR(Context *ctx) {
  auto *operand_type = ctx->type_of(operand.get());
  if (operand_type->is<type::Struct>() && dispatch_table_.total_size_ != 0) {
    // TODO struct is not exactly right. we really mean user-defined
    FnArgs<std::pair<Expression *, ir::Val>> args;
    args.pos_ = {std::pair(operand.get(), operand->EmitIR(ctx)[0])};
    return EmitCallDispatch(args, dispatch_table_, ctx->type_of(this), ctx);
  }

  switch (op) {
    case Language::Operator::Not: {
      auto *t = ctx->type_of(operand.get());
      if (t == type::Bool) {
        return {ir::ValFrom(ir::Not(operand->EmitIR(ctx)[0].reg_or<bool>()))};
      } else if (t->is<type::Flags>()) {
        auto *flags_type = &t->as<type::Flags>();
        return {ir::ValFrom(
            ir::Not(type::Typed<ir::RegisterOr<ir::FlagsVal>, type::Flags>(
                operand->EmitIR(ctx)[0].reg_or<ir::FlagsVal>(), flags_type)),
            flags_type)};
      } else {
        NOT_YET(); 
      }
    } break;
    case Language::Operator::Sub: {
      auto operand_ir = operand->EmitIR(ctx)[0];
      return {type::ApplyTypes<i8, i16, i32, i64, float, double>(
          ctx->type_of(operand.get()), [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::ValFrom(ir::Neg(operand_ir.reg_or<T>()));
          })};
    } break;
    case Language::Operator::TypeOf:
      return {ir::Val(ctx->type_of(operand.get()))};
    case Language::Operator::Which:
      return {ir::Val::Reg(
          ir::Load<type::Type const *>(ir::VariantType(
              std::get<ir::Register>(operand->EmitIR(ctx)[0].value))),
          type::Type_)};
    case Language::Operator::And:
      return {ir::Val::Reg(operand->EmitLVal(ctx)[0],
                           type::Ptr(ctx->type_of(this)))};
    case Language::Operator::Eval: {
      // TODO what if there's an error during evaluation?
      return backend::Evaluate(operand.get(), ctx);
    }
    case Language::Operator::Mul:
      return {ir::ValFrom(
          ir::Ptr(operand->EmitIR(ctx)[0].reg_or<type::Type const *>()))};
    case Language::Operator::At: {
      auto *t = ctx->type_of(this);
      return {ir::Val::Reg(
          ir::Load(std::get<ir::Register>(operand->EmitIR(ctx)[0].value), t),
          t)};
    }
    case Language::Operator::Needs: {
      // TODO validate requirements are well-formed?
      ir::Func::Current->precondition_exprs_.push_back(operand.get());
      return {};
    } break;
    case Language::Operator::Ensure: {
      // TODO validate requirements are well-formed?
      ir::Func::Current->postcondition_exprs_.push_back(operand.get());
      return {};
    } break;
    case Language::Operator::Pass: return operand->EmitIR(ctx);
    default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}

base::vector<ir::Register> Unop::EmitLVal(Context *ctx) {
  ASSERT(op == Language::Operator::At);
  return {std::get<ir::Register>(operand->EmitIR(ctx)[0].value)};
}
}  // namespace ast
