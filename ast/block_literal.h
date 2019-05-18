#ifndef ICARUS_AST_BLOCK_LITERAL_H
#define ICARUS_AST_BLOCK_LITERAL_H

#include "ast/expression.h"
#include "ast/declaration.h"

namespace ast {
struct BlockLiteral : public Expression {
  BlockLiteral(bool required) : required_(required) {}
  ~BlockLiteral() override {}

#include "visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    std::stringstream ss;
    ss << "block" << (required_ ? "" : "?") << " {\n";
    for (auto const &b : before_) {
      ss << std::string(2 * (n + 1), ' ') << b.to_string(n + 1) << "\n";
    }
    for (auto const &a : after_) {
      ss << std::string(2 * (n + 1), ' ') << a.to_string(n + 1) << "\n";
    }
    ss << std::string(2 * n, ' ') << "}";
    return ss.str();
  }

  std::vector<Declaration> before_, after_;
  std::unique_ptr<core::Scope> body_scope_;
  bool required_;
};
}  // namespace ast

#endif  // ICARUS_AST_BLOCK_LITERAL_H
