#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include "absl/container/flat_hash_set.h"
#include "ast/node.h"
#include "ast/scope/scope.h"
#include "ir/value/hashtag.h"

namespace ast {

struct Expression : public Node {
  Expression(frontend::SourceRange const &span = frontend::SourceRange())
      : Node(span) {}

  Expression(Expression &&) noexcept      = default;
  Expression(Expression const &) noexcept = default;
  Expression &operator=(Expression &&) noexcept = default;
  Expression &operator=(Expression const &) noexcept = default;

  virtual ~Expression() {}

  bool parenthesized_ = false;

  absl::flat_hash_set<ir::Hashtag> hashtags;
};

}  // namespace ast

#endif  // ICARUS_AST_EXPRESSION_H
