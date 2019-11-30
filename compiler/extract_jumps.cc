#include "compiler/extract_jumps.h"

#include "ast/ast.h"

namespace compiler {

absl::Span<ast::Node const *const> ExtractJumps::jumps(
    ExtractJumps::Kind k) const {
  return data_[static_cast<std::underlying_type_t<Kind>>(k)];
}

void ExtractJumps::Visit(ast::Access const *node) { Visit(node->operand()); }

void ExtractJumps::Visit(ast::ArrayLiteral const *node) {
  for (auto const *expr : node->elems()) { Visit(expr); }
}

void ExtractJumps::Visit(ast::ArrayType const *node) {
  for (auto const &len : node->lengths()) { Visit(len); }
  Visit(node->data_type());
}

void ExtractJumps::Visit(ast::Binop const *node) {
  Visit(node->lhs());
  Visit(node->rhs());
}

void ExtractJumps::Visit(ast::BlockLiteral const *node) {
  for (auto const *b : node->before()) { Visit(b); }
  for (auto const *a : node->after()) { Visit(a); }
}

void ExtractJumps::Visit(ast::BlockNode const *node) {
  for (auto const *stmt : node->stmts()) { Visit(stmt); }
}

void ExtractJumps::Visit(ast::BuiltinFn const *node) {}

void ExtractJumps::Visit(ast::Call const *node) {
  Visit(node->callee());
  for (ast::Expression const *expr : node->args()) { Visit(expr); }
}

void ExtractJumps::Visit(ast::Cast const *node) {
  Visit(node->expr());
  Visit(node->type());
}

void ExtractJumps::Visit(ast::ChainOp const *node) {
  for (auto *expr : node->exprs()) { Visit(expr); }
}

void ExtractJumps::Visit(ast::CommaList const *node) {
  for (auto &expr : node->exprs_) { Visit(expr.get()); }
}

void ExtractJumps::Visit(ast::Declaration const *node) {
  if (node->type_expr()) { Visit(node->type_expr()); }
  if (node->init_val()) { Visit(node->init_val()); }
}

void ExtractJumps::Visit(ast::EnumLiteral const *node) {
  for (auto const *elem : node->elems()) { Visit(elem); }
}

void ExtractJumps::Visit(ast::FunctionLiteral const *node) {
  for (auto const &param : node->params()) { Visit(param.value.get()); }
  auto outputs = node->outputs();
  if (not outputs) { return; }
  for (auto *out : *outputs) { Visit(out); }
}

void ExtractJumps::Visit(ast::Identifier const *node) {}

void ExtractJumps::Visit(ast::Import const *node) { Visit(node->operand()); }

void ExtractJumps::Visit(ast::Index const *node) {
  Visit(node->lhs());
  Visit(node->rhs());
}

void ExtractJumps::Visit(ast::Goto const *node) {
  // TODO Can you return or yield or jump from inside a jump block?!
  for (auto const &opt : node->options()) {
    for (std::unique_ptr<ast::Expression> const &expr : opt.args()) {
      Visit(expr.get());
    }
  }
  data_[static_cast<std::underlying_type_t<Kind>>(Kind::Jump)].push_back(node);
}

void ExtractJumps::Visit(ast::Jump const *node) {
  // TODO Can you return or yield or jump from inside a jump block?!
  for (auto const &param : node->params()) { Visit(param.value.get()); }
  for (auto const *stmt : node->stmts()) { Visit(stmt); }
}

void ExtractJumps::Visit(ast::PrintStmt const *node) {
  for (auto *expr : node->exprs()) { Visit(expr); }
}

void ExtractJumps::Visit(ast::ReturnStmt const *node) {
  for (auto *expr : node->exprs()) { Visit(expr); }
  constexpr auto key = static_cast<std::underlying_type_t<Kind>>(Kind::Return);
  data_[key].push_back(node);
}

void ExtractJumps::Visit(ast::YieldStmt const *node) {
  for (auto *expr : node->exprs()) { Visit(expr); }
  constexpr auto key = static_cast<std::underlying_type_t<Kind>>(Kind::Yield);
  data_[key].push_back(node);
}

void ExtractJumps::Visit(ast::ScopeLiteral const *node) {
  for (auto const *decl : node->decls()) { Visit(decl); }
}

void ExtractJumps::Visit(ast::ScopeNode const *node) {
  Visit(node->name());
  for (auto const *expr : node->args()) { Visit(expr); }
  for (auto const &block : node->blocks()) { Visit(&block); }
}

void ExtractJumps::Visit(ast::StructLiteral const *node) {
  for (auto &a : node->args_) { Visit(&a); }
  for (auto &f : node->fields_) { Visit(&f); }
}

void ExtractJumps::Visit(ast::StructType const *node) {
  for (auto &arg : node->args_) { Visit(arg.get()); }
}

void ExtractJumps::Visit(ast::Switch const *node) {
  if (node->expr_) { Visit(node->expr_.get()); }
  for (auto & [ body, cond ] : node->cases_) {
    Visit(body.get());
    Visit(cond.get());
  }
}

void ExtractJumps::Visit(ast::Terminal const *node) {}

void ExtractJumps::Visit(ast::Unop const *node) { Visit(node->operand()); }

}  // namespace compiler
