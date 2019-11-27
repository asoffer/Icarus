#include "module/dependent_decls.h"

#include "ast/ast.h"

namespace module {

void DependentDecls::Visit(ast::Access const *node, ast::Declaration const *d) {
  Visit(node->operand(), d);
}

void DependentDecls::Visit(ast::ArrayLiteral const *node,
                           ast::Declaration const *d) {
  for (auto const *expr : node->elems()) { Visit(expr, d); }
}

void DependentDecls::Visit(ast::ArrayType const *node,
                           ast::Declaration const *d) {
  for (auto const &len : node->lengths()) { Visit(len, d); }
  Visit(node->data_type(), d);
}

void DependentDecls::Visit(ast::Binop const *node, ast::Declaration const *d) {
  Visit(node->lhs(), d);
  Visit(node->rhs(), d);
}

void DependentDecls::Visit(ast::BlockLiteral const *node,
                           ast::Declaration const *d) {
  for (auto const *b : node->before()) { Visit(b, d); }
  for (auto const *a : node->after()) { Visit(a, d); }
}

void DependentDecls::Visit(ast::BlockNode const *node,
                           ast::Declaration const *d) {
  for (auto const *stmt : node->stmts()) { Visit(stmt, d); }
}

void DependentDecls::Visit(ast::BuiltinFn const *node,
                           ast::Declaration const *d) {}

void DependentDecls::Visit(ast::Call const *node, ast::Declaration const *d) {
  Visit(node->callee(), d);
  for (ast::Expression const *expr : node->args()) { Visit(expr, d); }
}

void DependentDecls::Visit(ast::Cast const *node, ast::Declaration const *d) {
  Visit(node->expr(), d);
  Visit(node->type(), d);
}

void DependentDecls::Visit(ast::ChainOp const *node,
                           ast::Declaration const *d) {
  for (auto const *expr : node->exprs()) { Visit(expr, d); }
}

void DependentDecls::Visit(ast::CommaList const *node,
                           ast::Declaration const *d) {
  for (auto const &expr : node->exprs_) { Visit(expr.get(), d); }
}

void DependentDecls::Visit(ast::Declaration const *node,
                           ast::Declaration const *d) {
  decl_graph_.graph_.add_edge(d, node);
  if (node->type_expr()) { Visit(node->type_expr(), node); }
  if (node->init_val()) { Visit(node->init_val(), node); }
}

void DependentDecls::Visit(ast::EnumLiteral const *node,
                           ast::Declaration const *d) {
  for (auto const *elem : node->elems()) { Visit(elem, d); }
}

void DependentDecls::Visit(ast::FunctionLiteral const *node,
                           ast::Declaration const *d) {
  for (auto const &param : node->params()) { Visit(param.value.get(), d); }
  if (auto outputs = node->outputs()) {
    for (auto const &out : *outputs) { Visit(out, d); }
  }
}

void DependentDecls::Visit(ast::Identifier const *node,
                           ast::Declaration const *d) {
  decl_graph_.ids_[node->token()].push_back(d);
}

void DependentDecls::Visit(ast::Import const *node, ast::Declaration const *d) {
  Visit(node->operand(), d);
}

void DependentDecls::Visit(ast::Index const *node, ast::Declaration const *d) {
  Visit(node->lhs(), d);
  Visit(node->rhs(), d);
}

void DependentDecls::Visit(ast::Goto const *node, ast::Declaration const *d) {
  for (auto const &opt : node->options()) {
    for (std::unique_ptr<ast::Expression> const &expr : opt.args()) {
      Visit(expr.get(), d);
    }
  }
}

void DependentDecls::Visit(ast::Jump const *node,
                           ast::Declaration const *d) {
  for (auto const &param : node->params()) { Visit(param.value.get(), d); }
  for (auto const *stmt : node->stmts()) { Visit(stmt, d); }
}

void DependentDecls::Visit(ast::PrintStmt const *node,
                           ast::Declaration const *d) {
  for (auto *expr : node->exprs()) { Visit(expr, d); }
}

void DependentDecls::Visit(ast::ReturnStmt const *node,
                           ast::Declaration const *d) {
  for (auto *expr : node->exprs()) { Visit(expr, d); }
}

void DependentDecls::Visit(ast::YieldStmt const *node,
                           ast::Declaration const *d) {
  for (auto *expr : node->exprs()) { Visit(expr, d); }
}

void DependentDecls::Visit(ast::ScopeLiteral const *node,
                           ast::Declaration const *d) {
  for (auto const *decl : node->decls()) { Visit(decl, d); }
}

void DependentDecls::Visit(ast::ScopeNode const *node,
                           ast::Declaration const *d) {
  Visit(node->name(), d);
  for (ast::Expression const *expr : node->args()) { Visit(expr, d); }
  for (auto const &block : node->blocks()) { Visit(&block, d); }
}

void DependentDecls::Visit(ast::StructLiteral const *node,
                           ast::Declaration const *d) {
  for (auto &a : node->args_) { Visit(&a, d); }
  for (auto &f : node->fields_) { Visit(&f, d); }
}

void DependentDecls::Visit(ast::StructType const *node,
                           ast::Declaration const *d) {
  for (auto &arg : node->args_) { Visit(arg.get(), d); }
}

void DependentDecls::Visit(ast::Switch const *node, ast::Declaration const *d) {
  if (node->expr_) { Visit(node->expr_.get(), d); }
  for (auto &[body, cond] : node->cases_) {
    Visit(body.get(), d);
    Visit(cond.get(), d);
  }
}

void DependentDecls::Visit(ast::Terminal const *node,
                           ast::Declaration const *d) {}

void DependentDecls::Visit(ast::Unop const *node, ast::Declaration const *d) {
  Visit(node->operand(), d);
}

}  // namespace module
