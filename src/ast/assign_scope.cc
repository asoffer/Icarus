#include "ast.h"
#include "stages.h"
#include "ast/identifier.h"

namespace AST {
void Access::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  operand->assign_scope(scope);
}

void Binop::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  lhs->assign_scope(scope);
  rhs->assign_scope(scope);
}

void Declaration::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  ASSERT(scope != nullptr);
  scope_ = scope;
  scope_->InsertDecl(this);
  identifier->assign_scope(scope);
  if (type_expr) { type_expr->assign_scope(scope); }
  if (init_val) { init_val->assign_scope(scope); }
}

void ChainOp::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  for (auto &expr : exprs) { expr->assign_scope(scope); }
}

void CommaList::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  for (auto &expr : exprs) { expr->assign_scope(scope); }
}

void GenericFunctionLiteral::assign_scope(Scope *scope) {
  FunctionLiteral::assign_scope(scope);
}

void FunctionLiteral::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  if (!fn_scope) {
    fn_scope         = scope->add_child<FnScope>();
    fn_scope->fn_lit = this;
  }
  for (auto &in : inputs) { in->assign_scope(fn_scope.get()); }
  for (auto &out : outputs) { out->assign_scope(fn_scope.get()); }
  statements->assign_scope(fn_scope.get());
}

void ScopeLiteral::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_     = scope;
  body_scope = scope->add_child<DeclScope>();
  if (enter_fn) { enter_fn->assign_scope(body_scope.get()); }
  if (exit_fn) { exit_fn->assign_scope(body_scope.get()); }
}

void StructLiteral::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_     = scope;
  type_scope = scope->add_child<DeclScope>();
  for (auto &f : fields_) { f->assign_scope(type_scope.get()); }
}
}  // namespace AST
