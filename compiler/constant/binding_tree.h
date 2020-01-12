#ifndef ICARUS_COMPILER_CONSTANT_BINDING_TREE_H
#define ICARUS_COMPILER_CONSTANT_BINDING_TREE_H

#include <memory>

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
    explicit Node(Node const* parent) : parent_(parent) {}

    Node const* parent() const { return parent_; }
    ConstantBinding const& binding() const { return binding_; }
    ConstantBinding& binding() { return binding_; }

   private:
    ConstantBinding binding_;
    Node const* parent_ = nullptr;
  };

  ConstantBindingTree() {
    // Initialize the root.
    AddChildTo(nullptr);
  }

  Node const* root() const { return nodes_[0].get(); }
  Node* root() { return nodes_[0].get(); }

  Node* AddChildTo(Node const* parent) {
    nodes_.push_back(std::make_unique<Node>(parent));
    return nodes_.back().get();
  }

 private:
  std::vector<std::unique_ptr<Node>> nodes_;
};

}  // namespace compiler

#endif  //  ICARUS_COMPILER_CONSTANT_BINDING_TREE_H
