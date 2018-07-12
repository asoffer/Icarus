#include "ast/unop.h"

#include "ast/fn_args.h"
#include "ast/terminal.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "base/check.h"
#include "context.h"
#include "ir/func.h"
#include "type/all.h"

base::vector<IR::Val> EmitCallDispatch(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx);

IR::Val PtrCallFix(const IR::Val& v);

void ForEachExpr(AST::Expression *expr,
                 const std::function<void(size_t, AST::Expression *)> &fn);

namespace AST {
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
    case Language::Operator::Generate: ss << "generate "; break;
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
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  operand->assign_scope(scope);
}

void Unop::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  operand->Validate(ctx);
}

void Unop::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  if (op == Language::Operator::Ref) {
    // TODO need to extract the right module here
    Context ctx(nullptr);
    operand->assign_scope(scope);
    operand->VerifyType(&ctx);
    operand->Validate(&ctx);
    auto val = operand->EmitIR(&ctx)[0];

    args->push_back(val);
    args->push_back(IR::Val::Ref(this));
  } else {
    operand->SaveReferences(scope, args);
  }
}

void Unop::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  if (op == Language::Operator::Ref) {
    auto iter = replacements.find(&correspondant->as<Unop>());
    ASSERT(iter != replacements.end());
    auto terminal    = std::make_unique<Terminal>();
    terminal->scope_ = scope_; // TODO Eh? Do I care?
    terminal->span   = span;
    terminal->lvalue = lvalue; // TODO????
    terminal->type   = iter->second.type;
    terminal->value  = iter->second;
    operand          = std::move(terminal);
    op               = Language::Operator::Pass;
  } else {
    operand->contextualize(correspondant->as<Unop>().operand.get(),
                           replacements);
  }
}

void Unop::ExtractReturns(base::vector<const Expression *> *rets) const {
  operand->ExtractReturns(rets);
}

Unop *Unop::Clone() const {
  auto *result            = new Unop;
  result->span            = span;
  result->operand         = base::wrap_unique(operand->Clone());
  result->op              = op;
  result->dispatch_table_ = dispatch_table_;
  return result;
}

void Unop::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  VERIFY_AND_RETURN_ON_ERROR(operand);

  using Language::Operator;

  if (op != Operator::At && op != Operator::And) {
    lvalue = operand->lvalue == Assign::Const ? Assign::Const : Assign::LVal;
  }

  switch (op) {
    case Operator::TypeOf:
      type   = type::Type_;
      lvalue = Assign::Const;
      break;
    case Operator::Eval:
      type   = operand->type;
      lvalue = Assign::Const;
      if (operand->lvalue != Assign::Const) {
        ctx->error_log_.EvaluatingNonConstant(span);
        limit_to(StageRange::NoEmitIR());
      }
      break;
    case Operator::Generate: type = type::Void(); break;
    case Operator::Which: {
      type   = type::Type_;
      lvalue = operand->lvalue == Assign::Const ? Assign::Const : Assign::RVal;
      if (!operand->type->is<type::Variant>()) {
        ctx->error_log_.WhichNonVariant(operand->type, span);
        limit_to(StageRange::NoEmitIR());
      }
    } break;
    case Operator::At: {
      lvalue = Assign::LVal;
      if (operand->type->is<type::Pointer>()) {
        type = operand->type->as<type::Pointer>().pointee;

      } else {
        ctx->error_log_.DereferencingNonPointer(operand->type, span);
        type = type::Err;
        limit_to(StageRange::Nothing());
      }
    } break;
    case Operator::And: {
      switch (operand->lvalue) {
        case Assign::Const:
          ctx->error_log_.TakingAddressOfConstant(span);
          lvalue = Assign::RVal;
          break;
        case Assign::RVal:
          ctx->error_log_.TakingAddressOfTemporary(span);
          lvalue = Assign::RVal;
          break;
        case Assign::LVal: break;
        case Assign::Unset: UNREACHABLE();
      }
      type = Ptr(operand->type);
    } break;
    case Operator::Mul: {
      limit_to(operand);
      if (operand->type != type::Type_) {
        NOT_YET("log an error");
        type = type::Err;
        limit_to(StageRange::Nothing());
      } else {
        type = type::Type_;
      }
    } break;
    case Operator::Sub: {
      if (operand->type == type::Int || operand->type == type::Real) {
        type = operand->type;

      } else if (operand->type->is<type::Struct>()) {
        FnArgs<Expression *> args;
        args.pos_ = base::vector<Expression*>{operand.get()};
        std::tie(dispatch_table_, type) =
            DispatchTable::Make(args, "-", scope_, ctx);
        ASSERT(type, Not(Is<type::Tuple>()));
        if (type == type::Err) { limit_to(StageRange::Nothing()); }
      }
    } break;
    case Operator::Not: {
      if (operand->type == type::Bool) {
        type = type::Bool;
      } else if (operand->type->is<type::Struct>()) {
        FnArgs<Expression *> args;
        args.pos_ = base::vector<Expression*>{operand.get()};
        std::tie(dispatch_table_, type) =
            DispatchTable::Make(args, "!", scope_, ctx);
        ASSERT(type, Not(Is<type::Tuple>()));
        if (type == type::Err) { limit_to(StageRange::Nothing()); }
      } else {
        NOT_YET("log an error");
        type = type::Err;
        limit_to(StageRange::Nothing());
      }
    } break;
    case Operator::Needs: {
      type = type::Void();
      if (operand->type != type::Bool) {
        ctx->error_log_.PreconditionNeedsBool(operand.get());
        limit_to(StageRange::NoEmitIR());
      }
    } break;
    case Operator::Ensure: {
      type = type::Void();
      if (operand->type != type::Bool) {
        ctx->error_log_.PostconditionNeedsBool(operand.get());
        limit_to(StageRange::NoEmitIR());
      }
    } break;
    case Operator::Pass: type = operand->type; break;
    default: UNREACHABLE(*this);
  }
  limit_to(operand);
}

base::vector<IR::Val> Unop::EmitIR(Context *ctx) {
  if (operand->type->is<type::Struct>() && dispatch_table_.total_size_ != 0) {
    // TODO struct is not exactly right. we really mean user-defined
    FnArgs<std::pair<Expression *, IR::Val>> args;
    args.pos_ = {
        std::pair(operand.get(), operand->type->is_big()
                                     ? PtrCallFix(operand->EmitIR(ctx)[0])
                                     : operand->EmitIR(ctx)[0])};
    return EmitCallDispatch(args, dispatch_table_, type, ctx);
  }

  switch (op) {
    case Language::Operator::Not: return {IR::Not(operand->EmitIR(ctx)[0])};
    case Language::Operator::Sub:
      if (operand->type == type::Int) {
        return {IR::NegInt(operand->EmitIR(ctx)[0])};
      } else if (operand->type == type::Real) {
        return {IR::NegReal(operand->EmitIR(ctx)[0])};
      } else {
        UNREACHABLE();
      }
    case Language::Operator::TypeOf: return {IR::Val::Type(operand->type)};
    case Language::Operator::Which:
      if (lvalue == Assign::Const) {
        NOT_YET();
      } else {
        return {IR::Load(IR::VariantType(operand->EmitIR(ctx)[0]))};
      }
    case Language::Operator::And: return {operand->EmitLVal(ctx)[0]};
    case Language::Operator::Eval: {
      // TODO what if there's an error during evaluation?
      // TODO what about ``a, b = $FnWithMultipleReturnValues()``
      return backend::Evaluate(operand.get(), ctx);
    }
    case Language::Operator::Generate: {
      auto val = backend::Evaluate(operand.get(), ctx).at(0);
      ASSERT(val.type == type::Code);
      auto block = std::get<AST::CodeBlock>(val.value);
      if (auto *err = std::get_if<std::string>(&block.content_)) {
        ctx->error_log_.UserDefinedError(*err);
        return {};
      }

      auto *stmts = &std::get<AST::Statements>(block.content_);
      stmts->assign_scope(scope_);
      stmts->VerifyType(ctx);
      stmts->Validate(ctx);
      return stmts->EmitIR(ctx);

    } break;
    case Language::Operator::Mul: return {IR::Ptr(operand->EmitIR(ctx)[0])};
    case Language::Operator::At: return {PtrCallFix(operand->EmitIR(ctx)[0])};
    case Language::Operator::Needs: {
      // TODO validate requirements are well-formed?
      IR::Func::Current->precondition_exprs_.push_back(operand.get());
      return {};
    } break;
    case Language::Operator::Ensure: {
      // TODO validate requirements are well-formed?
      IR::Func::Current->postcondition_exprs_.push_back(operand.get());
      return {};
    } break;
    case Language::Operator::Pass: return operand->EmitIR(ctx);
    default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}

base::vector<IR::Val> Unop::EmitLVal(Context *ctx) {
  ASSERT(op == Language::Operator::At);
  return operand->EmitIR(ctx);
}
}  // namespace AST
