#include "ast/repeated_unop.h"

#include "ast/fn_args.h"
#include "ast/function_literal.h"
#include "ast/verify_macros.h"
#include "context.h"
#include "ir/func.h"
#include "scope.h"
#include "type/all.h"

base::vector<IR::Val> EmitCallDispatch(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx);

IR::Val PtrCallFix(const IR::Val& v);

void ForEachExpr(AST::Expression *expr,
                 const std::function<void(size_t, AST::Expression *)> &fn);

namespace AST {
std::string RepeatedUnop::to_string(size_t n) const {
  switch (op_) {
    case Language::Operator::Return: return "return " + args_.to_string(n);
    case Language::Operator::Print: return "print " + args_.to_string(n);
    default: { UNREACHABLE(); }
  }
}

void RepeatedUnop::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  args_.assign_scope(scope);
}

void RepeatedUnop::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  args_.Validate(ctx);
}

void RepeatedUnop::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  args_.SaveReferences(scope, args);
}

void RepeatedUnop::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < args_.exprs.size(); ++i) {
    args_.contextualize(&correspondant->as<RepeatedUnop>().args_, replacements);
  }
}

void RepeatedUnop::ExtractReturns(base::vector<const Expression *> *rets) const {
  args_.ExtractReturns(rets);
  if (op_ == Language::Operator::Return) { rets->push_back(&args_); }
}

RepeatedUnop *RepeatedUnop::Clone() const {
  auto *result = new RepeatedUnop;
  result->span = span;
  for (const auto &arg : args_.exprs) {
    result->args_.exprs.emplace_back(arg->Clone());
  }
  result->op_              = op_;
  result->dispatch_tables_ = dispatch_tables_;
  return result;
}

void RepeatedUnop::VerifyType(Context *ctx) {
  STAGE_CHECK(StartTypeVerificationStage, DoneTypeVerificationStage);
  args_.VerifyType(ctx);
  if (args_.type == type::Err) { return; }

  if (op_ == Language::Operator::Print) {
    ASSERT(dispatch_tables_.size() == args_.exprs.size());
    for (size_t i = 0; i < args_.exprs.size(); ++i) {
      auto &arg = args_.exprs[i];
      if (arg->type->is<type::Primitive>() || arg->type->is<type::Pointer>() ||
          arg->type->is<type::CharBuffer>()) {
        continue;
      } else if (arg->type->is<type::Struct>()) {
        FnArgs<Expression *> args;
        args.pos_.push_back(arg.get());
        const type::Type *ret_type = nullptr;
        std::tie(dispatch_tables_[i], ret_type) =
            DispatchTable::Make(args, "print", scope_, ctx);
        if (ret_type != type::Void()) {
          NOT_YET("log an error: ", ret_type);
          limit_to(StageRange::Nothing());
        }
      } else if (arg->type->is<type::Variant>()) {
        // TODO
      } else {
        NOT_YET(arg->type);
      }
    }
  }
}

base::vector<IR::Val> RepeatedUnop::EmitIR(Context *ctx) {
  auto arg_vals = args_.EmitIR(ctx);
  switch (op_) {
    case Language::Operator::Return: {
      size_t offset = 0;
      auto *fn_scope = ASSERT_NOT_NULL(scope_->ContainingFnScope());
      auto *fn_lit   = ASSERT_NOT_NULL(fn_scope->fn_lit);
      auto *fn_type  = &ASSERT_NOT_NULL(fn_lit->type)->as<type::Function>();
      for (size_t i = 0; i < args_.exprs.size(); ++i) {
        // TODO return type maybe not the same as type actually returned?
        IR::SetReturn(i, arg_vals[i]);
      }
      IR::ReturnJump();
      return {};
    }
    case Language::Operator::Print:
      for (size_t i = 0; i < args_.exprs.size(); ++i) {
        if (args_.exprs[i]->type->is<type::Primitive>() ||
            args_.exprs[i]->type->is<type::Pointer>() ||
            args_.exprs[i]->type->is<type::CharBuffer>()) {
          IR::Print(arg_vals[i]);
        } else if (args_.exprs[i]->type->is<type::Struct>()) {
          ASSERT(dispatch_tables_[i].total_size_ != 0u);
          // TODO struct is not exactly right. we really mean user-defined
          FnArgs<std::pair<Expression *, IR::Val>> args;
          args.pos_ = {
              std::pair(args_.exprs[i].get(), args_.exprs[i]->type->is_big()
                                                  ? PtrCallFix(arg_vals[i])
                                                  : arg_vals[i])};
          EmitCallDispatch(args, dispatch_tables_[i], type::Void(), ctx);
        } else {
          args_.exprs[i]->type->EmitRepr(arg_vals[i], ctx);
        }
      }
      return {};
    default: UNREACHABLE("Operator is ", static_cast<int>(op_));
  }
}
}  // namespace AST
