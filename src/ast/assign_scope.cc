#include "ast.h"

namespace AST {
static constexpr int ThisStage() { return 0; }

void Unop::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  operand->assign_scope(scope);
}

void Access::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  operand->assign_scope(scope);
}

void Identifier::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
}

void Terminal::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  if (type !=type::Type_) { return; }
}

void ArrayType::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  length->assign_scope(scope);
  data_type->assign_scope(scope);
}

void For::assign_scope(Scope *scope) {
  STAGE_CHECK;
  if (!for_scope) { for_scope = scope->add_child<ExecScope>(); }
  for_scope->can_jump = true;
  for (auto &iter : iterators) { iter->assign_scope(for_scope.get()); }
  statements->assign_scope(for_scope.get());
}

void ArrayLiteral::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  for (auto &el : elems) { el->assign_scope(scope); }
}

void Binop::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  lhs->assign_scope(scope);
  if (rhs) { rhs->assign_scope(scope); }
}

void InDecl::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  scope_->InsertDecl(this);
  identifier->assign_scope(scope);
  container->assign_scope(scope);
}

void Declaration::assign_scope(Scope *scope) {
  STAGE_CHECK;
  ASSERT(scope, "");
  scope_ = scope;
  scope_->InsertDecl(this);
  identifier->assign_scope(scope);
  if (type_expr) { type_expr->assign_scope(scope); }
  if (init_val) { init_val->assign_scope(scope); }
}

void ChainOp::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  for (auto &expr : exprs) { expr->assign_scope(scope); }
}

void CommaList::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  for (auto &expr : exprs) { expr->assign_scope(scope); }
}

void Statements::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  for (auto &stmt : content_) { stmt->assign_scope(scope); }
}

void GenericFunctionLiteral::assign_scope(Scope *scope) {
  FunctionLiteral::assign_scope(scope);
}

void FunctionLiteral::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  if (!fn_scope) {
    fn_scope         = scope->add_child<FnScope>();
    fn_scope->fn_lit = this;
  }
  if (return_type_expr) { return_type_expr->assign_scope(fn_scope.get()); }
  for (auto &in : inputs) { in->assign_scope(fn_scope.get()); }
  statements->assign_scope(fn_scope.get());
}

void Jump::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
}
void CodeBlock::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
}

void Call::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  fn_->assign_scope(scope);
  args_.Apply([scope](auto &expr) { expr->assign_scope(scope); });
}

void ScopeNode::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
  if (!internal) { internal = scope_->add_child<ExecScope>(); }
  scope_expr->assign_scope(scope);
  if (expr) { expr->assign_scope(scope); }
  stmts->assign_scope(internal.get());
}

void ScopeLiteral::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_     = scope;
  body_scope = scope->add_child<ExecScope>();
  if (enter_fn) { enter_fn->assign_scope(body_scope.get()); }
  if (exit_fn) { exit_fn->assign_scope(body_scope.get()); }
}

void StructLiteral::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_     = scope;
  type_scope = scope->add_child<DeclScope>();
  for (auto &f : fields_) { f->assign_scope(type_scope.get()); }
}

void Hole::assign_scope(Scope *scope) {
  STAGE_CHECK;
  scope_ = scope;
}
} // namespace AST
