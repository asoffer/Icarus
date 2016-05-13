#ifndef ICARUS_UNITY
#include "Scope.h"
#endif

namespace AST {
void Unop::assign_scope() {
  scope_ = CurrentScope();
  operand->assign_scope();
}
void Access::assign_scope() {
  scope_ = CurrentScope();
  operand->assign_scope();
}

void Identifier::assign_scope() { scope_ = CurrentScope(); }
void Terminal::assign_scope() { scope_ = CurrentScope(); }
void ArrayType::assign_scope() { scope_ = CurrentScope(); }
void EnumLiteral::assign_scope() { scope_ = CurrentScope(); }

void Conditional::assign_scope() {
  scope_ = CurrentScope();
  for (size_t i = 0; i < conditions.size(); ++i) {
    body_scopes[i]->set_parent(CurrentScope());
    conditions[i]->assign_scope();

    Scope::Stack.push(body_scopes[i]);
    statements[i]->assign_scope();
    Scope::Stack.pop();
  }

  if (has_else()) {
    body_scopes.back()->set_parent(CurrentScope());
    Scope::Stack.push(body_scopes.back());
    statements.back()->assign_scope();
    Scope::Stack.pop();
  }
}

void For::assign_scope() {
  scope_ = CurrentScope();
  for_scope->set_parent(CurrentScope());

  Scope::Stack.push(for_scope);

  for (auto iter : iterators) { iter->assign_scope(); }
  statements->assign_scope();

  Scope::Stack.pop();
}

void While::assign_scope() {
  scope_ = CurrentScope();
  while_scope->set_parent(CurrentScope());
  condition->assign_scope();
  Scope::Stack.push(while_scope);
  statements->assign_scope();
  Scope::Stack.pop();
}

void ArrayLiteral::assign_scope() {
  scope_ = CurrentScope();
  for (auto &el : elems) { el->assign_scope(); }
}

void Binop::assign_scope() {
  scope_ = CurrentScope();
  lhs->assign_scope();
  rhs->assign_scope();
}

void InDecl::assign_scope() {
  scope_ = CurrentScope();
  // TODO this only works if they're in identical scopes. If there's a parent
  // scope relationship, we fail.
  auto iter = scope_->ids_.find(identifier->token());
  if (iter == scope_->ids_.end()) {
    scope_->ids_[identifier->token()] = identifier;
  }

  // TODO Shouldn't we register this declaration?
  // scope_->ids_[identifier->token()]->decls.push_back(this);

  identifier->assign_scope();
  container->assign_scope();
}

void Declaration::assign_scope() {
  scope_ = CurrentScope();
  // TODO this only works if they're in identical scopes. If there's a parent
  // scope relationship, we fail.
  auto iter = scope_->ids_.find(identifier->token());
  if (iter == scope_->ids_.end()) {
    scope_->ids_[identifier->token()] = identifier;
  }
  scope_->ids_[identifier->token()]->decls.push_back(this);

  identifier->assign_scope();
  type_expr->assign_scope();
}

void ChainOp::assign_scope() {
  scope_ = CurrentScope();
  for (auto &expr : exprs) { expr->assign_scope(); }
}

void Case::assign_scope() {
  scope_ = CurrentScope();
  for (auto &kv : key_vals) {
    kv.first->assign_scope();
    kv.second->assign_scope();
  }
}

void Statements::assign_scope() {
  scope_ = CurrentScope();
  for (auto &nptr : statements) { nptr->assign_scope(); }
}

void FunctionLiteral::assign_scope() {
  scope_ = CurrentScope();
  fn_scope->set_parent(CurrentScope());

  Scope::Stack.push(fn_scope);
  for (auto &in : inputs) { in->assign_scope(); }
  statements->assign_scope();
  Scope::Stack.pop();
}

void StructLiteral::assign_scope() {
  scope_ = CurrentScope();
  type_scope->set_parent(CurrentScope());

  Scope::Stack.push(type_scope);
  for (auto &param : params) { param->assign_scope(); }
  for (auto &decl : declarations) { decl->assign_scope(); }
  Scope::Stack.pop();
}

void Jump::assign_scope() { scope_ = CurrentScope(); }
void DummyTypeExpr::assign_scope() { scope_ = CurrentScope(); }
} // namespace AST
