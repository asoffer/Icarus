#ifndef ICARUS_COMPILER_CONSTANT_BINDING_MAP_H
#define ICARUS_COMPILER_CONSTANT_BINDING_MAP_H

#include "absl/container/flat_hash_map.h"
#include "compiler/constant/binding_tree.h"

namespace compiler {

template <typename T>
struct ConstantBindingMap {
  ConstantBindingMap() { map_.emplace(tree_.root(), std::make_unique<T>()); }
  T const& root_value() { return *map_.find(tree_.root())->second; }

  void emplace(ConstantBindingTree::Node const* node, T val) {
    map_[node] = std::make_unique<T>(std::move(val));
  }

  ConstantBindingTree::Node const* root() const { return tree_.root(); }

 private:
  absl::flat_hash_map<ConstantBindingTree::Node const*, std::unique_ptr<T>>
      map_;
  ConstantBindingTree tree_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_CONSTANT_BINDING_MAP_H
