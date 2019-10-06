#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include "ast/expr_ptr.h"
#include "ast/hashtag.h"
#include "ast/node.h"
#include "core/scope.h"

namespace ast {

struct Expression : public Node {
  Expression(frontend::SourceRange const &span = frontend::SourceRange())
      : Node(span) {}

  Expression(Expression &&) noexcept      = default;
  Expression(Expression const &) noexcept = default;
  Expression &operator=(Expression &&) noexcept = default;
  Expression &operator=(Expression const &) noexcept = default;

  virtual ~Expression() {}

  module::Module *module() const { return scope_->module(); }

  virtual bool needs_expansion() const { return false; }
  std::vector<Hashtag> hashtags_;
  bool parenthesized_ = false;

  bool contains_hashtag(Hashtag needle) const {
    for (auto const &tag : hashtags_) {
      if (tag.kind_ == needle.kind_) { return true; }
    }
    return false;
  }
};

}  // namespace ast

#endif  // ICARUS_AST_EXPRESSION_H
