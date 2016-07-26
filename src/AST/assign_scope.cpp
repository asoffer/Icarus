#ifndef ICARUS_UNITY
#include "Scope.h"
#endif

#define ITERATE_OR_SKIP(container)                                             \
  for (auto ptr : container) {                                                 \
    if (!ptr) { continue; }                                                    \
    ptr->assign_scope();                                                       \
  }


// TODO these are not threadsafe! When we access the stack, when compilation is
// multi-threaded, we should probably grab a mutex before getting the top of the
// stack

std::stack<Scope *> ScopeStack;
static Scope *CurrentScope() {
  return ScopeStack.empty() ? nullptr : ScopeStack.top();
}

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

void ArrayType::assign_scope() {
  scope_ = CurrentScope();
  length->assign_scope();
  data_type->assign_scope();
}

void Conditional::assign_scope() {
  scope_ = CurrentScope();
  for (size_t i = 0; i < conditions.size(); ++i) {
    body_scopes[i]->set_parent(CurrentScope());
    conditions[i]->assign_scope();

    ScopeStack.push(body_scopes[i]);
    statements[i]->assign_scope();
    ScopeStack.pop();
  }

  if (has_else()) {
    body_scopes.back()->set_parent(CurrentScope());
    ScopeStack.push(body_scopes.back());
    statements.back()->assign_scope();
    ScopeStack.pop();
  }
}

void For::assign_scope() {
  scope_ = CurrentScope();
  for_scope->set_parent(CurrentScope());

  ScopeStack.push(for_scope);

  for (auto iter : iterators) { iter->assign_scope(); }
  statements->assign_scope();

  ScopeStack.pop();
}

void While::assign_scope() {
  scope_ = CurrentScope();
  while_scope->set_parent(CurrentScope());
  condition->assign_scope();
  ScopeStack.push(while_scope);
  statements->assign_scope();
  ScopeStack.pop();
}

void ArrayLiteral::assign_scope() {
  scope_ = CurrentScope();
  for (auto &el : elems) { el->assign_scope(); }
}

void Binop::assign_scope() {
  scope_ = CurrentScope();
  lhs->assign_scope();
  if (rhs) { rhs->assign_scope(); }
}

void Generic::assign_scope() {
  scope_ = CurrentScope();
  scope_->DeclRegistry.push_back(this);
  identifier->assign_scope();
  test_fn->assign_scope();
}

void InDecl::assign_scope() {
  scope_ = CurrentScope();
  scope_->DeclRegistry.push_back(this);
  identifier->assign_scope();
  container->assign_scope();
}

void Declaration::assign_scope() {
  scope_ = CurrentScope();
  scope_->DeclRegistry.push_back(this);
  identifier->assign_scope();
  if (type_expr) { type_expr->assign_scope(); }
  if (init_val) { init_val->assign_scope(); }
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
  ScopeStack.push(fn_scope);
  return_type_expr->assign_scope();
  for (auto &in : inputs) { in->assign_scope(); }
  statements->assign_scope();
  ScopeStack.pop();
}

void ParametricStructLiteral::assign_scope() {
  scope_ = CurrentScope();
  type_scope->set_parent(CurrentScope());

  ScopeStack.push(type_scope);
  for (auto p : params) { p->assign_scope(); }
  for (auto d : decls) { d->assign_scope(); }
  ScopeStack.pop();
}

void StructLiteral::assign_scope() {
  scope_ = CurrentScope();
  type_scope->set_parent(CurrentScope());

  ScopeStack.push(type_scope);
  for (auto d : decls) { d->assign_scope(); }
  ScopeStack.pop();
}

void Jump::assign_scope() { scope_ = CurrentScope(); }
void DummyTypeExpr::assign_scope() { scope_ = CurrentScope(); }
} // namespace AST
#undef ITERATE_OR_SKIP
