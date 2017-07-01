#include "ast.h"

#include "../ir/ir.h"
#include "../scope.h"
#include "../type/type.h"

namespace AST {
void Unop::assign_scope(Scope *scope) {
  scope_ = scope;
  operand->assign_scope(scope);
}

void Access::assign_scope(Scope *scope) {
  scope_ = scope;
  operand->assign_scope(scope);
}

void Identifier::assign_scope(Scope *scope) { scope_ = scope; }
void Terminal::assign_scope(Scope *scope) { 
  scope_ = scope;
  if (type != Type_) { return; }
  if (value.as_type->is<ParamStruct>()) {
    auto ps = static_cast<ParamStruct *>(value.as_type);
    if (!ps->type_scope) { ps->type_scope = scope->add_child<DeclScope>(); }

    for (auto p : ps->params) { p->assign_scope(ps->type_scope); }
    for (auto d : ps->decls) { d->assign_scope(ps->type_scope); }

  } else if (value.as_type->is<Struct>()) {
    auto s = static_cast<Struct *>(value.as_type);
    if (!s->type_scope) { s->type_scope = scope->add_child<DeclScope>(); }
    for (auto d : s->decls) { d->assign_scope(s->type_scope); }
  }
  // TODO enum type?
}

void ArrayType::assign_scope(Scope *scope) {
  scope_ = scope;
  length->assign_scope(scope);
  data_type->assign_scope(scope);
}

void For::assign_scope(Scope *scope) {
  if (!for_scope) { for_scope = scope->add_child<ExecScope>(); }
  for_scope->can_jump = true;
  for (auto it : iterators) { it->assign_scope(for_scope); }
  statements->assign_scope(for_scope);
}

void ArrayLiteral::assign_scope(Scope *scope) {
  scope_ = scope;
  for (auto &el : elems) { el->assign_scope(scope); }
}

void Binop::assign_scope(Scope *scope) {
  scope_ = scope;
  lhs->assign_scope(scope);
  if (rhs) { rhs->assign_scope(scope); }
}

void Generic::assign_scope(Scope *scope) {
  scope_ = scope;
  scope_->decls_.push_back(this);
  identifier->assign_scope(scope);
  test_fn->assign_scope(scope);
}

void InDecl::assign_scope(Scope *scope) {
  scope_ = scope;
  scope_->decls_.push_back(this);
  identifier->assign_scope(scope);
  container->assign_scope(scope);
}

void Declaration::assign_scope(Scope *scope) {
  ASSERT(scope, "");
  scope_ = scope;
  scope_->decls_.push_back(this);
  identifier->assign_scope(scope);
  if (type_expr) { type_expr->assign_scope(scope); }
  if (init_val) { init_val->assign_scope(scope); }
}

void ChainOp::assign_scope(Scope *scope) {
  scope_ = scope;
  for (auto &expr : exprs) { expr->assign_scope(scope); }
}

void Case::assign_scope(Scope *scope) {
  scope_ = scope;
  for (auto &kv : key_vals) {
    kv.first->assign_scope(scope);
    kv.second->assign_scope(scope);
  }
}

void Statements::assign_scope(Scope *scope) {
  scope_ = scope;
  for (auto nptr : statements) { nptr->assign_scope(scope); }
}

void FunctionLiteral::assign_scope(Scope *scope) {
  scope_ = scope;
  if (!fn_scope) {
    fn_scope         = scope->add_child<FnScope>();
    fn_scope->fn_lit = this;
  }

  return_type_expr->assign_scope(fn_scope);
  for (auto &in : inputs) { in->assign_scope(fn_scope); }
  statements->assign_scope(fn_scope);
}

void Jump::assign_scope(Scope *scope) { scope_ = scope; }
void CodeBlock::assign_scope(Scope *scope) { scope_ = scope; }

void ScopeNode::assign_scope(Scope *scope) {
  scope_ = scope;
  if (!internal) { internal = scope_->add_child<ExecScope>(); }
  scope_expr->assign_scope(scope);
  if (expr) { expr->assign_scope(scope); }
  stmts->assign_scope(internal);
}

void ScopeLiteral::assign_scope(Scope *scope) {
  scope_ = scope;
  body_scope = scope->add_child<ExecScope>();
  if (enter_fn != nullptr) { enter_fn->assign_scope(body_scope); }
  if (exit_fn != nullptr) { exit_fn->assign_scope(body_scope); }
}
} // namespace AST
