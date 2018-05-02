#include "ast/scope_literal.h"

#include "ast/declaration.h"
#include "ast/verify_macros.h"
#include "error/log.h"
#include "ir/val.h"
#include "scope.h"
#include "type/function.h"
#include "type/scope.h"

namespace AST {
std::string ScopeLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "scope {\n"
     << std::string(2 * (n + 1), ' ') << enter_fn->to_string(n + 1) << "\n"
     << std::string(2 * (n + 1), ' ') << exit_fn->to_string(n + 1) << "\n"
     << std::string(2 * n, ' ') << "}";
  return ss.str();
}

void ScopeLiteral::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_     = scope;
  body_scope = scope->add_child<DeclScope>();
  if (enter_fn) { enter_fn->assign_scope(body_scope.get()); }
  if (exit_fn) { exit_fn->assign_scope(body_scope.get()); }
}

void ScopeLiteral::ClearIdDecls() {
  stage_range_ = StageRange{};
  if (enter_fn) { enter_fn->ClearIdDecls(); }
  if (exit_fn) { exit_fn->ClearIdDecls(); }
}

void ScopeLiteral::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  lvalue                            = Assign::Const;
  bool cannot_proceed_due_to_errors = false;
  if (!enter_fn) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(
        this->span, "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
  }

  if (!exit_fn) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(
        this->span, "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
  }

  if (cannot_proceed_due_to_errors) {
    limit_to(StageRange::Nothing());
    return;
  }

  VERIFY_AND_RETURN_ON_ERROR(enter_fn);
  VERIFY_AND_RETURN_ON_ERROR(exit_fn);

  if (!enter_fn->type->is<type::Function>()) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(
        this->span, "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
  }

  if (!exit_fn->type->is<type::Function>()) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(
        this->span, "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
  }

  if (cannot_proceed_due_to_errors) {
    limit_to(StageRange::Nothing());
  } else {
    type = type::Scp(enter_fn->type->as<type::Function>().input);
  }
}

void ScopeLiteral::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  // TODO
}

void ScopeLiteral::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  enter_fn->SaveReferences(scope, args);
  exit_fn->SaveReferences(scope, args);
}

void ScopeLiteral::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
  enter_fn->contextualize(correspondant->as<ScopeLiteral>().enter_fn.get(),
                          replacements);
  exit_fn->contextualize(correspondant->as<ScopeLiteral>().exit_fn.get(),
                         replacements);
}

void ScopeLiteral::ExtractReturns(std::vector<const Expression *> *rets) const {
  if (enter_fn) { enter_fn->ExtractReturns(rets); }
  if (exit_fn) { exit_fn->ExtractReturns(rets); }
}

ScopeLiteral *ScopeLiteral::Clone() const {
  auto *result     = new ScopeLiteral;
  result->span     = span;
  result->enter_fn = base::wrap_unique(enter_fn->Clone());
  result->exit_fn  = base::wrap_unique(exit_fn->Clone());
  return result;
}

IR::Val AST::ScopeLiteral::EmitIR(Context *ctx) {
  enter_fn->init_val->EmitIR(ctx);
  exit_fn->init_val->EmitIR(ctx);
  return IR::Val::Scope(this);
}

}  // namespace AST
