#include "AST.h"

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

void While::assign_scope() {
  scope_ = CurrentScope();
  while_scope->set_parent(CurrentScope());
  Scope::Stack.push(while_scope);
  condition->assign_scope();
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

void Declaration::assign_scope() {
  scope_ = CurrentScope();
  scope_->ids_[identifier->token()] = identifier;
  identifier->decl = this;

  identifier->assign_scope();
  type_expr->assign_scope();
}

void ChainOp::assign_scope() {
  scope_ = CurrentScope();
  for (auto &expr : exprs) { expr->assign_scope(); }
}

void Case::assign_scope() {
  scope_ = CurrentScope();
  kv->assign_scope();
}

void KVPairList::assign_scope() {
  scope_ = CurrentScope();
  for (auto &pair : pairs) {
    pair.first->assign_scope();
    pair.second->assign_scope();
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

void TypeLiteral::assign_scope() {
  scope_ = CurrentScope();
  type_scope->set_parent(CurrentScope());

  Scope::Stack.push(type_scope);
  for (auto &decl : declarations) { decl->assign_scope(); }
  Scope::Stack.pop();
}
} // namespace AST
