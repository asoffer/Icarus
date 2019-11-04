#include "format/token_extractor.h"

#include "ast/ast.h"

namespace format {

template <typename T>
static void Join(TokenExtractor *visitor, base::PtrSpan<T const> span,
                 std::string_view joiner) {
  if (not span.empty()) {
    using std::begin;
    using std::end;
    auto iter = begin(span);
    (*iter)->ExtractTokens(visitor);
    ++iter;
    for (; iter != end(span); ++iter) {
      visitor->line_builder_.write(joiner);
      (*iter)->ExtractTokens(visitor);
    }
  }
}

void TokenExtractor::operator()(ast::Access const *node) {
  node->operand()->ExtractTokens(this);
  line_builder_.write(".");
  line_builder_.write(node->member_name());
}

void TokenExtractor::operator()(ast::ArrayLiteral const *node) {
  line_builder_.write("[");
  Join(this, node->elems(), ",");
  line_builder_.write("]");
}

void TokenExtractor::operator()(ast::ArrayType const *node) {
  line_builder_.write("[");
  Join(this, node->lengths(), ",");
  line_builder_.write(";");
  node->data_type()->ExtractTokens(this);
  line_builder_.write("]");
}

void TokenExtractor::operator()(ast::Binop const *node) {
  node->lhs()->ExtractTokens(this);
  line_builder_.write(stringify(node->op()));
  node->rhs()->ExtractTokens(this);
}

void TokenExtractor::operator()(ast::BlockLiteral const *node) {
  UNREACHABLE();
}

void TokenExtractor::operator()(ast::BlockNode const *node) { UNREACHABLE(); }

void TokenExtractor::operator()(ast::Call const *node) {
  node->callee()->ExtractTokens(this);
  line_builder_.write("(");
  // TODO Join(this, node->args(), ",");
  line_builder_.write(")");
}

void TokenExtractor::operator()(ast::Cast const *node) {
  node->expr()->ExtractTokens(this);
  line_builder_.write("as");
  node->type()->ExtractTokens(this);
}

void TokenExtractor::operator()(ast::ChainOp const *node) {
  // TODO op
  for (auto *expr : node->exprs()) { expr->ExtractTokens(this); }
}

void TokenExtractor::operator()(ast::CommaList const *node) {
  // TODO Join(this, node->exprs_, ",");
}
///////////////////////////////////////////////////////////////////////////
void TokenExtractor::operator()(ast::Declaration const *node) {
  if (node->type_expr()) { node->type_expr()->ExtractTokens(this); }
  if (node->init_val()) { node->init_val()->ExtractTokens(this); }
}

void TokenExtractor::operator()(ast::EnumLiteral const *node) {
  Join(this, node->elems(), "\n");
}

void TokenExtractor::operator()(ast::FunctionLiteral const *node) {
  // TODO
}

void TokenExtractor::operator()(ast::Import const *node) {
  node->operand()->ExtractTokens(this);
}

void TokenExtractor::operator()(ast::Index const *node) {
  node->lhs()->ExtractTokens(this);
  node->rhs()->ExtractTokens(this);
}

void TokenExtractor::operator()(ast::Jump const *node) {
  for (auto &opt : node->options_) {
    for (auto &expr : opt.args) { expr->ExtractTokens(this); }
  }
}

void TokenExtractor::operator()(ast::JumpHandler const *node) {
  Join(this, node->input(), ",");
  Join(this, node->stmts(), "\n");
}

void TokenExtractor::operator()(ast::PrintStmt const *node) {
  Join(this, node->exprs(), ",");
}

void TokenExtractor::operator()(ast::ReturnStmt const *node) {
  Join(this, node->exprs(), ",");
}

void TokenExtractor::operator()(ast::YieldStmt const *node) {
  Join(this, node->exprs(), ",");
}

void TokenExtractor::operator()(ast::ScopeLiteral const *node) {
  for (auto *decl : node->decls()) { decl->ExtractTokens(this); }
}

void TokenExtractor::operator()(ast::ScopeNode const *node) {
  node->name()->ExtractTokens(this);
  // TODO

  for (auto &block : node->blocks()) { block.ExtractTokens(this); }
}

void TokenExtractor::operator()(ast::StructLiteral const *node) {
  for (auto &a : node->args_) { a.ExtractTokens(this); }
  for (auto &f : node->fields_) { f.ExtractTokens(this); }
}

void TokenExtractor::operator()(ast::StructType const *node) {
  for (auto &arg : node->args_) { arg->ExtractTokens(this); }
}

void TokenExtractor::operator()(ast::Switch const *node) {
  if (node->expr_) { node->expr_->ExtractTokens(this); }
  for (auto &[body, cond] : node->cases_) {
    body->ExtractTokens(this);
    cond->ExtractTokens(this);
  }
}

void TokenExtractor::operator()(ast::Unop const *node) {
  // TODO
  node->operand()->ExtractTokens(this);
}

void TokenExtractor::operator()(ast::Identifier const *node) {
  line_builder_.write(node->token());
}
void TokenExtractor::operator()(ast::Terminal const *node) {
  line_builder_.write("TERM");
}
void TokenExtractor::operator()(ast::BuiltinFn const *node) { UNREACHABLE(); }
}  // namespace format
