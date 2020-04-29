#ifndef ICARUS_COMPILER_CONSTANT_BINDING_MAP_H
#define ICARUS_COMPILER_CONSTANT_BINDING_MAP_H

#include "absl/container/flat_hash_map.h"
#include "compiler/constant/binding_tree.h"

namespace compiler {

template <typename T>
struct ConstantBindingMap {
  template <typename... Args>
  ConstantBindingMap(Args&&... args) {
    map_.emplace(tree_.root(),
                 std::make_unique<T>(std::forward<Args>(args)...));
  }

  T const& root_value() const { return *map_.find(tree_.root())->second; }
  T& root_value() { return *map_.find(tree_.root())->second; }

  template <typename... Args>
  ConstantBindingTree::Node* AddChildTo(ConstantBindingTree::Node* node,
                                        Args&&... args) {
    auto* result = tree_.AddChildTo(node);
    map_.emplace(result, std::make_unique<T>(std::forward<Args>(args)...));
    return result;
  }

  template <typename... Args>
  void try_emplace(ConstantBindingTree::Node* node, Args&&... args) {
    map_[node] = std::make_unique<T>(std::forward<Args>(args)...);
  }

  ConstantBindingTree::Node const* root() const { return tree_.root(); }
  ConstantBindingTree::Node* root() { return tree_.root(); }

 private:
  absl::flat_hash_map<ConstantBindingTree::Node const*, std::unique_ptr<T>>
      map_;
  ConstantBindingTree tree_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_CONSTANT_BINDING_MAP_H
