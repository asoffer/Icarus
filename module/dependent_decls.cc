#include "module/dependent_decls.h"

#include "ast/ast.h"

namespace module {

void DependentDecls::operator()(ast::Access const *node,
                                ast::Declaration const *d) {
  node->operand()->DependentDecls(this, d);
}

void DependentDecls::operator()(ast::ArrayLiteral const *node,
                                ast::Declaration const *d) {
  for (auto const *expr : node->elems()) { expr->DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::ArrayType const *node,
                                ast::Declaration const *d) {
  for (auto const &len : node->lengths()) { len->DependentDecls(this, d); }
  node->data_type()->DependentDecls(this, d);
}

void DependentDecls::operator()(ast::Binop const *node,
                                ast::Declaration const *d) {
  node->lhs()->DependentDecls(this, d);
  node->rhs()->DependentDecls(this, d);
}

void DependentDecls::operator()(ast::BlockLiteral const *node,
                                ast::Declaration const *d) {
  for (auto const *b : node->before()) { b->DependentDecls(this, d); }
  for (auto const *a : node->after()) { a->DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::BlockNode const *node,
                                ast::Declaration const *d) {
  for (auto const *stmt : node->stmts()) { stmt->DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::BuiltinFn const *node,
                                ast::Declaration const *d) {}

void DependentDecls::operator()(ast::Call const *node,
                                ast::Declaration const *d) {
  node->callee()->DependentDecls(this, d);
  for (ast::Expression const *expr : node->args()) {
    expr->DependentDecls(this, d);
  }
}

void DependentDecls::operator()(ast::Cast const *node,
                                ast::Declaration const *d) {
  node->expr()->DependentDecls(this, d);
  node->type()->DependentDecls(this, d);
}

void DependentDecls::operator()(ast::ChainOp const *node,
                                ast::Declaration const *d) {
  for (auto const *expr : node->exprs()) { expr->DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::CommaList const *node,
                                ast::Declaration const *d) {
  for (auto const &expr : node->exprs_) { expr->DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::Declaration const *node,
                                ast::Declaration const *d) {
  decl_graph_.graph_.add_edge(d, node);
  if (node->type_expr()) { node->type_expr()->DependentDecls(this, node); }
  if (node->init_val()) { node->init_val()->DependentDecls(this, node); }
}

void DependentDecls::operator()(ast::EnumLiteral const *node,
                                ast::Declaration const *d) {
  for (auto const *elem : node->elems()) { elem->DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::FunctionLiteral const *node,
                                ast::Declaration const *d) {
  for (auto const &in : node->inputs_) { in.value->DependentDecls(this, d); }
  if (node->outputs_) {
    for (auto const &out : *node->outputs_) { out->DependentDecls(this, d); }
  }
}

void DependentDecls::operator()(ast::Identifier const *node,
                                ast::Declaration const *d) {
  decl_graph_.ids_[node->token()].push_back(d);
}

void DependentDecls::operator()(ast::Import const *node,
                                ast::Declaration const *d) {
  node->operand()->DependentDecls(this, d);
}

void DependentDecls::operator()(ast::Index const *node,
                                ast::Declaration const *d) {
  node->lhs()->DependentDecls(this, d);
  node->rhs()->DependentDecls(this, d);
}

void DependentDecls::operator()(ast::Jump const *node,
                                ast::Declaration const *d) {
  for (auto const &opt : node->options_) {
    for (std::unique_ptr<ast::Expression> const &expr : opt.args) {
      expr->DependentDecls(this, d);
    }
  }
}

void DependentDecls::operator()(ast::JumpHandler const *node,
                                ast::Declaration const *d) {
  for (auto const *in : node->input()) { in->DependentDecls(this, d); }
  for (auto const *stmt : node->stmts()) { stmt->DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::PrintStmt const *node,
                                ast::Declaration const *d) {
  for (auto *expr : node->exprs()) { expr->DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::ReturnStmt const *node,
                                ast::Declaration const *d) {
  for (auto *expr : node->exprs()) { expr->DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::YieldStmt const *node,
                                ast::Declaration const *d) {
  for (auto *expr : node->exprs()) { expr->DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::ScopeLiteral const *node,
                                ast::Declaration const *d) {
  for (auto const *decl : node->decls()) { decl->DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::ScopeNode const *node,
                                ast::Declaration const *d) {
  node->name()->DependentDecls(this, d);
  for (ast::Expression const *expr : node->args()) {
    expr->DependentDecls(this, d);
  }
  for (auto const &block : node->blocks()) { block.DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::StructLiteral const *node,
                                ast::Declaration const *d) {
  for (auto &a : node->args_) { a.DependentDecls(this, d); }
  for (auto &f : node->fields_) { f.DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::StructType const *node,
                                ast::Declaration const *d) {
  for (auto &arg : node->args_) { arg->DependentDecls(this, d); }
}

void DependentDecls::operator()(ast::Switch const *node,
                                ast::Declaration const *d) {
  if (node->expr_) { node->expr_->DependentDecls(this, d); }
  for (auto &[body, cond] : node->cases_) {
    body->DependentDecls(this, d);
    cond->DependentDecls(this, d);
  }
}

void DependentDecls::operator()(ast::Terminal const *node,
                                ast::Declaration const *d) {}

void DependentDecls::operator()(ast::Unop const *node,
                                ast::Declaration const *d) {
  node->operand()->DependentDecls(this, d);
}

}  // namespace module
