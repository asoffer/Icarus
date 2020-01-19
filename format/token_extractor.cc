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
    visitor->Visit(*iter);
    ++iter;
    for (; iter != end(span); ++iter) {
      visitor->line_builder_.write(joiner);
      visitor->Visit(*iter);
    }
  }
}

void TokenExtractor::Visit(ast::Access const *node) {
  Visit(node->operand());
  line_builder_.write(".");
  line_builder_.write(node->member_name());
}

void TokenExtractor::Visit(ast::ArrayLiteral const *node) {
  line_builder_.write("[");
  Join(this, node->elems(), ",");
  line_builder_.write("]");
}

void TokenExtractor::Visit(ast::ArrayType const *node) {
  line_builder_.write("[");
  Join(this, node->lengths(), ",");
  line_builder_.write(";");
  Visit(node->data_type());
  line_builder_.write("]");
}

void TokenExtractor::Visit(ast::Binop const *node) {
  Visit(node->lhs());
  line_builder_.write(stringify(node->op()));
  Visit(node->rhs());
}

void TokenExtractor::Visit(ast::BlockLiteral const *node) { UNREACHABLE(); }

void TokenExtractor::Visit(ast::BlockNode const *node) { UNREACHABLE(); }

void TokenExtractor::Visit(ast::Call const *node) {
  Visit(node->callee());
  line_builder_.write("(");
  // TODO Join(this, node->args(), ",");
  line_builder_.write(")");
}

void TokenExtractor::Visit(ast::Cast const *node) {
  Visit(node->expr());
  line_builder_.write("as");
  Visit(node->type());
}

void TokenExtractor::Visit(ast::ChainOp const *node) {
  // TODO op
  for (auto *expr : node->exprs()) { Visit(expr); }
}

void TokenExtractor::Visit(ast::CommaList const *node) {
  // TODO Join(this, node->exprs_, ",");
}
///////////////////////////////////////////////////////////////////////////
void TokenExtractor::Visit(ast::Declaration const *node) {
  if (node->type_expr()) { Visit(node->type_expr()); }
  if (node->init_val()) { Visit(node->init_val()); }
}

void TokenExtractor::Visit(ast::EnumLiteral const *node) {
  Join(this, node->elems(), "\n");
}

void TokenExtractor::Visit(ast::FunctionLiteral const *node) {
  // TODO
}

void TokenExtractor::Visit(ast::Import const *node) { Visit(node->operand()); }

void TokenExtractor::Visit(ast::Index const *node) {
  Visit(node->lhs());
  Visit(node->rhs());
}

void TokenExtractor::Visit(ast::Goto const *node) {
  for (auto &opt : node->options()) {
    for (auto &expr : opt.args()) { Visit(expr.get()); }
  }
}

void TokenExtractor::Visit(ast::Jump const *node) {
  // TODO
}

void TokenExtractor::Visit(ast::PrintStmt const *node) {
  Join(this, node->exprs(), ",");
}

void TokenExtractor::Visit(ast::ReturnStmt const *node) {
  Join(this, node->exprs(), ",");
}

void TokenExtractor::Visit(ast::YieldStmt const *node) {
  Join(this, node->exprs(), ",");
}

void TokenExtractor::Visit(ast::ScopeLiteral const *node) {
  for (auto *decl : node->decls()) { Visit(decl); }
}

void TokenExtractor::Visit(ast::ScopeNode const *node) {
  Visit(node->name());
  // TODO

  for (auto &block : node->blocks()) { Visit(&block); }
}

void TokenExtractor::Visit(ast::StructLiteral const *node) {
  for (auto &f : node->fields()) { Visit(&f); }
}

void TokenExtractor::Visit(ast::ParameterizedStructLiteral const *node) {
  for (auto &a : node->params()) { Visit(&a); }
  for (auto &f : node->fields()) { Visit(&f); }
}

void TokenExtractor::Visit(ast::StructType const *node) {
  for (auto &arg : node->args_) { Visit(arg.get()); }
}

void TokenExtractor::Visit(ast::Switch const *node) {
  if (node->expr_) { Visit(node->expr_.get()); }
  for (auto &[body, cond] : node->cases_) {
    Visit(body.get());
    Visit(cond.get());
  }
}

void TokenExtractor::Visit(ast::Unop const *node) {
  // TODO
  Visit(node->operand());
}

void TokenExtractor::Visit(ast::DesignatedInitializer const *node) {
  NOT_YET();
}

void TokenExtractor::Visit(ast::Identifier const *node) {
  line_builder_.write(node->token());
}
void TokenExtractor::Visit(ast::Terminal const *node) {
  line_builder_.write("TERM");
}
void TokenExtractor::Visit(ast::BuiltinFn const *node) { UNREACHABLE(); }
}  // namespace format
