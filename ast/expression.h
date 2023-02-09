#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include <vector>

#include "absl/container/flat_hash_set.h"
#include "ast/node.h"
#include "data_types/hashtag.h"

namespace ast {

template <typename T>
concept ExpressionType =
    std::derived_from<T, Expression> or std::same_as<T, Expression>;

struct Expression : Node {
  Expression(int8_t which, std::string_view range) : Node(which, range) {}

  Expression(Expression &&) noexcept                 = default;
  Expression(Expression const &) noexcept            = default;
  Expression &operator=(Expression &&) noexcept      = default;
  Expression &operator=(Expression const &) noexcept = default;

  virtual ~Expression() {}

  absl::flat_hash_set<data_types::Hashtag> hashtags;

  // Add an extra layer of parentheses around this one.
  void wrap_parentheses(std::string_view range) {
    parentheses_.push_back(range);
  }

  // Returns the number of layers of parentheses wrapping this expression.
  size_t num_parentheses() const { return parentheses_.size(); }

 private:
  // Source locations of layers of parentheses. Earlier entries are inner
  // expressions.
  std::vector<std::string_view> parentheses_;
};

}  // namespace ast

#endif  // ICARUS_AST_EXPRESSION_H
