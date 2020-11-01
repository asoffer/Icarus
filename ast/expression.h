#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include "ast/hashtag.h"
#include "ast/node.h"
#include "ast/scope/scope.h"

namespace ast {

struct Expression : public Node {
  Expression(frontend::SourceRange const &span = frontend::SourceRange())
      : Node(span) {}

  Expression(Expression &&) noexcept      = default;
  Expression(Expression const &) noexcept = default;
  Expression &operator=(Expression &&) noexcept = default;
  Expression &operator=(Expression const &) noexcept = default;

  virtual ~Expression() {}

  std::vector<Hashtag> hashtags_;
  bool parenthesized_ = false;

  bool contains_hashtag(Hashtag const &h) const {
    for (auto const &tag : hashtags_) {
      if (tag == h) { return true; }
    }
    return false;
  }
};

}  // namespace ast

#endif  // ICARUS_AST_EXPRESSION_H
