#ifndef ICARUS_AST_MODULE_H
#define ICARUS_AST_MODULE_H

#include <concepts>
#include <memory>
#include <vector>

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/declaration.h"
#include "ast/node.h"
#include "ast/scope.h"
#include "base/ptr_span.h"

namespace ast {

// Module:
// Represents a module, the root node for a syntax tree.
struct Module : Node {
  // TODO: Remove `mod` parameter.
  explicit Module(module::Module *mod)
      : Node(IndexOf<Module>()), body_scope_(mod) {}

  Scope const &body_scope() const { return body_scope_; }
  Scope &body_scope() { return body_scope_; }

  // Inserts the nodes from the half-open [b, e) into the module, initializes
  // them, and returns a PtrSpan referencing those nodes. The returned span is
  // valid until the next non-const member function is called on `*this`.
  template <std::input_iterator Iter>
  base::PtrSpan<Node const> insert(Iter b, Iter e) {
    size_t prev_size = stmts_.size();

    Node::Initializer initializer{.scope = &body_scope()};
    while (b != e) {
      (*b)->Initialize(initializer);
      stmts_.push_back(std::move(*b++));
    }

    return base::PtrSpan<Node const>(stmts_.begin() + prev_size, stmts_.end());
  }

  base::PtrSpan<Node const> stmts() const { return stmts_; }

  void DebugStrAppend(std::string *out, size_t indent) const override {
    absl::StrAppend(out, absl::StrJoin(stmts(), "\n",
                                       [&](std::string *out, auto const &elem) {
                                         elem->DebugStrAppend(out, indent);
                                       }));
  }

  void Initialize(Initializer &initializer) override {
    initializer.scope = &body_scope();
    for (auto &n : stmts_) { n->Initialize(initializer); }
  }

 private:
  Scope body_scope_;
  std::vector<std::unique_ptr<Node>> stmts_;
};

}  // namespace ast

#endif  // ICARUS_AST_MODULE_H
