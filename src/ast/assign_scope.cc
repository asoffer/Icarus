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


}  // namespace AST
