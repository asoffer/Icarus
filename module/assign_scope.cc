#include "module/assign_scope.h"

#include "ast/ast.h"
#include "ast/scope/decl.h"
#include "module/dependent_decls.h"

namespace module {

template <typename T>
static void SetAllScopes(AssignScope *a, base::PtrSpan<T> span,
                         ast::Scope *scope) {
  for (auto *n : span) { a->Visit(n, scope); }
}

void AssignScope::To(base::PtrSpan<ast::Node> nodes, ast::Scope *scope) {
  AssignScope a;
  SetAllScopes(&a, nodes, scope);
}

void AssignScope::Visit(ast::Access *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->operand(), scope);
}

void AssignScope::Visit(ast::ArrayLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto *expr : node->elems()) { Visit(expr, scope); }
}

void AssignScope::Visit(ast::ArrayType *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto const &len : node->lengths()) { Visit(len, scope); }
  Visit(node->data_type(), scope);
}

void AssignScope::Visit(ast::Binop *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->lhs(), scope);
  Visit(node->rhs(), scope);
}

void AssignScope::Visit(ast::BlockLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  SetAllScopes(this, node->before(), node->body_scope());
  SetAllScopes(this, node->after(), node->body_scope());
}

void AssignScope::Visit(ast::BlockNode *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  for (auto &param : node->params()) {
    Visit(param.value.get(), node->body_scope());
  }
  SetAllScopes(this, node->stmts(), node->body_scope());
}

void AssignScope::Visit(ast::BuiltinFn *node, ast::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::Visit(ast::Call *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->callee(), scope);
  node->Apply([this, scope](ast::Expression *expr) { Visit(expr, scope); });
}

void AssignScope::Visit(ast::Cast *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->expr(), scope);
  Visit(node->type(), scope);
}

void AssignScope::Visit(ast::ChainOp *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto *expr : node->exprs()) { Visit(expr, scope); }
}

void AssignScope::Visit(ast::CommaList *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto &expr : node->exprs_) { Visit(expr.get(), scope); }
}

void AssignScope::Visit(ast::Declaration *node, ast::Scope *scope) {
  ASSERT(scope != nullptr);
  node->scope_ = scope;
  node->scope_->InsertDecl(node->id(), node);
  if (node->type_expr()) { Visit(node->type_expr(), scope); }
  if (node->init_val()) { Visit(node->init_val(), scope); }
}

void AssignScope::Visit(ast::DesignatedInitializer *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->type(), scope);
  for (auto &[field, expr] : node->assignments()) { Visit(expr.get(), scope); }
}

void AssignScope::Visit(ast::EnumLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  SetAllScopes(this, node->elems(), node->body_scope());
}

void AssignScope::Visit(ast::FunctionLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope, node);

  for (auto &param : node->params()) {
    Visit(param.value.get(), node->body_scope());
  }
  if (auto outputs = node->outputs()) {
    for (auto *out : *outputs) { Visit(out, node->body_scope()); }
  }
  SetAllScopes(this, node->stmts(), node->body_scope());
}

void AssignScope::Visit(ast::Identifier *node, ast::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::Visit(ast::Import *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->operand(), scope);
}

void AssignScope::Visit(ast::Index *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->lhs(), scope);
  Visit(node->rhs(), scope);
}

void AssignScope::Visit(ast::Goto *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto &opt : node->options()) {
    opt.args().Apply([this, scope](auto &expr) { Visit(expr.get(), scope); });
  }
}

void AssignScope::Visit(ast::Jump *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  if (node->state()) { Visit(node->state(), node->body_scope()); }
  for (auto &param : node->params()) {
    Visit(param.value.get(), node->body_scope());
  }
  SetAllScopes(this, node->stmts(), node->body_scope());
}

void AssignScope::Visit(ast::Label *node, ast::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::Visit(ast::ReturnStmt *node, ast::Scope *scope) {
  node->scope_ = scope;
  SetAllScopes(this, node->exprs(), scope);
}

void AssignScope::Visit(ast::YieldStmt *node, ast::Scope *scope) {
  node->scope_ = scope;
  SetAllScopes(this, node->exprs(), scope);
}

void AssignScope::Visit(ast::ScopeLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope, node);
  if (node->state_type()) { Visit(node->state_type(), scope); }
  for (auto *decl : node->decls()) { Visit(decl, node->body_scope()); }
}

void AssignScope::Visit(ast::ScopeNode *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->name(), scope);
  node->Apply([this, scope](ast::Expression *expr) { Visit(expr, scope); });

  for (auto &block : node->blocks()) { Visit(&block, scope); }
}

void AssignScope::Visit(ast::StructLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  for (auto &field : node->fields()) { Visit(&field, node->body_scope()); }
}

void AssignScope::Visit(ast::ParameterizedStructLiteral *node,
                        ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  for (auto &param : node->params()) { Visit(&param, node->body_scope()); }
  for (auto &field : node->fields()) { Visit(&field, node->body_scope()); }
}

void AssignScope::Visit(ast::StructType *node, ast::Scope *scope) {
  for (auto &arg : node->args_) { Visit(arg.get(), scope); }
}

void AssignScope::Visit(ast::Switch *node, ast::Scope *scope) {
  node->scope_ = scope;
  if (node->expr_) { Visit(node->expr_.get(), scope); }
  for (auto &[body, cond] : node->cases_) {
    Visit(body.get(), scope);
    Visit(cond.get(), scope);
  }
}

void AssignScope::Visit(ast::Terminal *node, ast::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::Visit(ast::Unop *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->operand(), scope);
}

}  // namespace module
