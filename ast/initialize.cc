#include "ast/initialize.h"

#include "ast/ast.h"
#include "ast/scope/decl.h"
// #include "module/dependent_decls.h"

namespace ast {

template <typename T>
static void SetAllScopes(Initialize *a, base::PtrSpan<T> span, Scope *scope) {
  for (auto *n : span) { a->Visit(n, scope); }
}

void InitializeNodes(base::PtrSpan<Node> nodes, Scope *scope) {
  Initialize i;
  SetAllScopes(&i, nodes, scope);
}

void Initialize::Visit(ast::Access *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->operand(), scope);
}

void Initialize::Visit(ArrayLiteral *node, Scope *scope) {
  node->scope_ = scope;
  for (auto *expr : node->elems()) { Visit(expr, scope); }
}

void Initialize::Visit(ArrayType *node, Scope *scope) {
  node->scope_ = scope;
  for (auto const &len : node->lengths()) { Visit(len, scope); }
  Visit(node->data_type(), scope);
}

void Initialize::Visit(Binop *node, Scope *scope) {
  node->scope_ = scope;
  Visit(node->lhs(), scope);
  Visit(node->rhs(), scope);
}

void Initialize::Visit(BlockLiteral *node, Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  SetAllScopes(this, node->before(), node->body_scope());
  SetAllScopes(this, node->after(), node->body_scope());
}

void Initialize::Visit(BlockNode *node, Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  for (auto &param : node->params()) {
    Visit(param.value.get(), node->body_scope());
  }
  SetAllScopes(this, node->stmts(), node->body_scope());
}

void Initialize::Visit(BuiltinFn *node, Scope *scope) { node->scope_ = scope; }

void Initialize::Visit(Call *node, Scope *scope) {
  node->scope_ = scope;
  Visit(node->callee(), scope);
  node->Apply([this, scope](Expression *expr) { Visit(expr, scope); });
}

void Initialize::Visit(Cast *node, Scope *scope) {
  node->scope_ = scope;
  Visit(node->expr(), scope);
  Visit(node->type(), scope);
}

void Initialize::Visit(ChainOp *node, Scope *scope) {
  node->scope_ = scope;
  for (auto *expr : node->exprs()) { Visit(expr, scope); }
}

void Initialize::Visit(CommaList *node, Scope *scope) {
  node->scope_ = scope;
  for (auto &expr : node->exprs_) { Visit(expr.get(), scope); }
}

void Initialize::Visit(Declaration *node, Scope *scope) {
  ASSERT(scope != nullptr);
  node->scope_ = scope;
  node->scope_->InsertDecl(node->id(), node);
  if (node->type_expr()) { Visit(node->type_expr(), scope); }
  if (node->init_val()) { Visit(node->init_val(), scope); }
}

void Initialize::Visit(DesignatedInitializer *node, Scope *scope) {
  node->scope_ = scope;
  Visit(node->type(), scope);
  for (auto &[field, expr] : node->assignments()) { Visit(expr.get(), scope); }
}

void Initialize::Visit(EnumLiteral *node, Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  SetAllScopes(this, node->elems(), node->body_scope());
}

void Initialize::Visit(FunctionLiteral *node, Scope *scope) {
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

void Initialize::Visit(Identifier *node, Scope *scope) { node->scope_ = scope; }

void Initialize::Visit(Import *node, Scope *scope) {
  node->scope_ = scope;
  Visit(node->operand(), scope);
}

void Initialize::Visit(Index *node, Scope *scope) {
  node->scope_ = scope;
  Visit(node->lhs(), scope);
  Visit(node->rhs(), scope);
}

void Initialize::Visit(Goto *node, Scope *scope) {
  node->scope_ = scope;
  for (auto &opt : node->options()) {
    opt.args().Apply([this, scope](auto &expr) { Visit(expr.get(), scope); });
  }
}

void Initialize::Visit(Jump *node, Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  if (node->state()) { Visit(node->state(), node->body_scope()); }
  for (auto &param : node->params()) {
    Visit(param.value.get(), node->body_scope());
  }
  SetAllScopes(this, node->stmts(), node->body_scope());
}

void Initialize::Visit(Label *node, Scope *scope) { node->scope_ = scope; }

void Initialize::Visit(ReturnStmt *node, Scope *scope) {
  node->scope_ = scope;
  SetAllScopes(this, node->exprs(), scope);
}

void Initialize::Visit(YieldStmt *node, Scope *scope) {
  node->scope_ = scope;
  SetAllScopes(this, node->exprs(), scope);
}

void Initialize::Visit(ScopeLiteral *node, Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope, node);
  if (node->state_type()) { Visit(node->state_type(), scope); }
  for (auto *decl : node->decls()) { Visit(decl, node->body_scope()); }
}

void Initialize::Visit(ScopeNode *node, Scope *scope) {
  node->scope_ = scope;
  Visit(node->name(), scope);
  node->Apply([this, scope](Expression *expr) { Visit(expr, scope); });

  for (auto &block : node->blocks()) { Visit(&block, scope); }
}

void Initialize::Visit(StructLiteral *node, Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  for (auto &field : node->fields()) { Visit(&field, node->body_scope()); }
}

void Initialize::Visit(ParameterizedStructLiteral *node, Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  for (auto &param : node->params()) { Visit(&param, node->body_scope()); }
  for (auto &field : node->fields()) { Visit(&field, node->body_scope()); }
}

void Initialize::Visit(StructType *node, Scope *scope) {
  for (auto &arg : node->args_) { Visit(arg.get(), scope); }
}

void Initialize::Visit(Switch *node, Scope *scope) {
  node->scope_ = scope;
  if (node->expr_) { Visit(node->expr_.get(), scope); }
  for (auto &[body, cond] : node->cases_) {
    Visit(body.get(), scope);
    Visit(cond.get(), scope);
  }
}

void Initialize::Visit(Terminal *node, Scope *scope) { node->scope_ = scope; }

void Initialize::Visit(Unop *node, Scope *scope) {
  node->scope_ = scope;
  Visit(node->operand(), scope);
}

}  // namespace ast
