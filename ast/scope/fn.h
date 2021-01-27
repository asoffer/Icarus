#ifndef ICARUS_AST_SCOPE_FN_H
#define ICARUS_AST_SCOPE_FN_H

#include <vector>

#include "absl/types/span.h"
#include "ast/scope/scope.h"

namespace ast {

// An executable scope representing the body of a function literal. These scopes
// need to know about all child scopes so they can stack-allocate enough space
// when they start.
struct FnScope : Scope {
  FnScope(Scope *parent) : Scope(parent, true) { descendants_.push_back(this); }

  bool is_visibility_boundary() const override { return true; }

  void insert_descendant(Scope *s) { descendants_.push_back(s); }
  absl::Span<Scope *const> descendants() const { return descendants_; }

 private:
  std::vector<Scope *> descendants_;
};

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_FN_H
