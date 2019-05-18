#ifndef ICARUS_AST_COMMA_LIST_H
#define ICARUS_AST_COMMA_LIST_H

#include "ast/expression.h"

namespace ast {
struct CommaList : public Expression {
  CommaList() = default;
  ~CommaList() override {}

  CommaList(CommaList const &) noexcept = default;
  CommaList(CommaList &&) noexcept      = default;
  CommaList &operator=(CommaList const &) noexcept = default;
  CommaList &operator=(CommaList &&) noexcept = default;

#include "visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    std::stringstream ss;
    if (exprs_.empty()) { return "()"; }
    if (parenthesized_) { ss << "("; }
    auto iter = exprs_.begin();
    ss << (*iter)->to_string(n);
    ++iter;
    while (iter != exprs_.end()) {
      ss << ", " << (*iter)->to_string(n);
      ++iter;
    }
    if (parenthesized_) { ss << ")"; }
    return ss.str();
  }

  bool needs_expansion() const override { return !parenthesized_; }

  std::vector<std::unique_ptr<Expression>> exprs_;
};
}  // namespace ast

#endif  // ICARUS_AST_COMMA_LIST_H
