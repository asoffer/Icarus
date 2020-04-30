#ifndef ICARUS_COMPILER_CONSTANT_BINDING_TREE_H
#define ICARUS_COMPILER_CONSTANT_BINDING_TREE_H

#include <memory>
#include <string>

#include "ast/ast_fwd.h"
#include "base/untyped_buffer_view.h"
#include "compiler/constant/binding.h"

namespace compiler {

// ConstantBindings are often dependent on constant parameters and so the same
// declaration may have been bound to different constants in different contexts.
// This means we need to form a tree with a node per generic instantiation.
// `ConstantBindingTree` is that tree.
//
// For example,
// ```
// f ::= (N :: int64, n: int64) -> () {
//    g ::= (M :: int64, m: int64) -> int64 {
//      return M + m
//    }
//    print_values( g(N, n), g(N * N, n * n) )
// }
//
// x: int64 = ...
// f(10, x)
// f(20, x)
// ```
//
// Note that when we encounter an instantiation of `f`, this may have many
// sub-instantiations of `g`. Thus, we need a tree-like data structure:
//
// +-[N = 10]
//   +-[M = 10]
//   +-[M = 100]
// +-[N = 20]
//   +-[M = 20]
//   +-[M = 400]
//
struct ConstantBindingTree {
  struct Node {
    explicit Node(Node* parent) : parent_(parent) {
#if defined(ICARUS_DEBUG)
      if (parent_) { ++parent_->num_children_; }
#endif
    }

    Node const* parent() const { return parent_; }
    Node* parent() { return parent_; }
    ConstantBinding const& binding() const { return binding_; }
    ConstantBinding& binding() { return binding_; }

    base::untyped_buffer_view find_constant(ast::Declaration const* decl);

    std::string DebugString() const;

#if defined(ICARUS_DEBUG)
    size_t num_children_ = 0;
#endif

   private:
    ConstantBinding binding_;
    Node* parent_ = nullptr;

  };

  ConstantBindingTree() {
    // Initialize the root.
    AddChildTo(nullptr);
  }

  Node const* root() const { return nodes_[0].get(); }
  Node* root() { return nodes_[0].get(); }

  Node* AddChildTo(Node* parent);

 private:
  std::vector<std::unique_ptr<Node>> nodes_;
};

}  // namespace compiler

#endif  //  ICARUS_COMPILER_CONSTANT_BINDING_TREE_H
