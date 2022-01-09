#ifndef ICARUS_AST_SCOPE_INTERVAL_H
#define ICARUS_AST_SCOPE_INTERVAL_H

#include "ast/scope.h"
#include "base/extend/absl_hash.h"
#include "base/extend/extend.h"

namespace ast {

// Represents a pair of scopes where one is an ancestor of the other.
struct ScopeInterval
    : base::Extend<ScopeInterval, 2>::With<base::AbslHashExtension> {
  explicit ScopeInterval(Scope const *descendant, Scope const *ancestor)
      : descendant_(ASSERT_NOT_NULL(descendant)),
        ancestor_(ASSERT_NOT_NULL(ancestor)) {}

  Scope::ancestor_iterator begin() const { return descendant_->begin(); }
  Scope::ancestor_iterator end() const { return ancestor_->parent().begin(); }

 private:
  Scope const *descendant_;
  Scope const *ancestor_;
};

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_INTERVAL_H
