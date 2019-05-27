#include "visitor/extract_jumps.h"

#include "ast/ast.h"

namespace visitor {

std::vector<ast::RepeatedUnop const *> const &ExtractJumps::jumps(
    ExtractJumps::Kind k) const {
  return data_[static_cast<std::underlying_type_t<Kind>>(k)];
}

void ExtractJumps::operator()(ast::Access const *node) {
  node->operand()->ExtractJumps(this);
}

void ExtractJumps::operator()(ast::ArrayLiteral const *node) {
  for (auto const *expr : node->elems()) { expr->ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::ArrayType const *node) {
  for (auto const &len : node->lengths()) { len->ExtractJumps(this); }
  node->data_type()->ExtractJumps(this);
}

void ExtractJumps::operator()(ast::Binop const *node) {
  node->lhs()->ExtractJumps(this);
  node->rhs()->ExtractJumps(this);
}

void ExtractJumps::operator()(ast::BlockLiteral const *node) {
  for (auto const *b : node->before()) { b->ExtractJumps(this); }
  for (auto const *a : node->after()) { a->ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::BlockNode const *node) {
  for (auto const *stmt : node->stmts()) { stmt->ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::BuiltinFn const *node) {}

void ExtractJumps::operator()(ast::Call const *node) {
  node->fn_->ExtractJumps(this);
  node->args_.Apply([this](std::unique_ptr<ast::Expression> const &expr) {
    expr->ExtractJumps(this);
  });
}

void ExtractJumps::operator()(ast::Cast const *node) {
  node->expr_->ExtractJumps(this);
  node->type_->ExtractJumps(this);
}

void ExtractJumps::operator()(ast::ChainOp const *node) {
  for (auto &expr : node->exprs) { expr->ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::CommaList const *node) {
  for (auto &expr : node->exprs_) { expr->ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::Declaration const *node) {
  if (node->type_expr) { node->type_expr->ExtractJumps(this); }
  if (node->init_val) { node->init_val->ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::EnumLiteral const *node) {
  for (auto &elem : node->elems_) { elem->ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::FunctionLiteral const *node) {
  for (auto &in : node->inputs_) { in.value->ExtractJumps(this); }
  for (auto &out : node->outputs_) { out->ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::Identifier const *node) {}

void ExtractJumps::operator()(ast::Import const *node) {
  node->operand_->ExtractJumps(this);
}

void ExtractJumps::operator()(ast::Index const *node) {
  node->lhs_->ExtractJumps(this);
  node->rhs_->ExtractJumps(this);
}

void ExtractJumps::operator()(ast::Interface const *node) {
  for (auto &d : node->decls_) { d.ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::RepeatedUnop const *node) {
  for (auto *expr : node->exprs()) { expr->ExtractJumps(this); }
  switch (node->op()) {
    case frontend::Operator::Jump:
      data_[static_cast<std::underlying_type_t<Kind>>(Kind::Jump)].push_back(
          node);
      break;
    case frontend::Operator::Return:
      data_[static_cast<std::underlying_type_t<Kind>>(Kind::Return)].push_back(
          node);
      break;
    case frontend::Operator::Yield:
      data_[static_cast<std::underlying_type_t<Kind>>(Kind::Yield)].push_back(
          node);
      break;
    default: break;
  }
}

void ExtractJumps::operator()(ast::ScopeLiteral const *node) {
  for (auto const *decl : node->decls()) { decl->ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::ScopeNode const *node) {
  for (auto &block : node->blocks_) { block.ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::StructLiteral const *node) {
  for (auto &a : node->args_) { a.ExtractJumps(this); }
  for (auto &f : node->fields_) { f.ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::StructType const *node) {
  for (auto &arg : node->args_) { arg->ExtractJumps(this); }
}

void ExtractJumps::operator()(ast::Switch const *node) {
  if (node->expr_) { node->expr_->ExtractJumps(this); }
  for (auto &[body, cond] : node->cases_) {
    body->ExtractJumps(this);
    cond->ExtractJumps(this);
  }
}

void ExtractJumps::operator()(ast::SwitchWhen const *node) { UNREACHABLE(); }

void ExtractJumps::operator()(ast::Terminal const *node) {}

void ExtractJumps::operator()(ast::Unop const *node) {
  node->operand->ExtractJumps(this);
}

}  // namespace visitor

