#ifndef ICARUS_AST_BLOCK_LITERAL_H
#define ICARUS_AST_BLOCK_LITERAL_H

#include "ast/expression.h"
#include "ast/declaration.h"

namespace ast {
struct BlockLiteral : public Expression {
  BlockLiteral(bool required);
  ~BlockLiteral() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  std::vector<Declaration> before_, after_;
  std::unique_ptr<core::Scope> body_scope_;
  bool required_;
};
}  // namespace ast

#endif  // ICARUS_AST_BLOCK_LITERAL_H
