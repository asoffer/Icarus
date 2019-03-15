#ifndef ICARUS_AST_NODE_LOOKUP_H
#define ICARUS_AST_NODE_LOOKUP_H

#include "absl/container/flat_hash_map.h"

namespace ast {
struct Node;
struct Identifier;

template <typename T>
struct NodeLookup {
  template <typename... Args>
  auto emplace(Args &&... args) {
    return data_.emplace(std::forward<Args>(args)...);
  }

  // TODO handle buffered data separately?
  template <typename... Args>
  auto buffered_emplace(Args &&... args) {
    return data_.emplace(std::forward<Args>(args)...);
  }

  auto at(Node const *n) const { return data_.at(n); }

  absl::flat_hash_map<Node const *, T> data_;
};
}  // namespace ast

#endif  // ICARUS_AST_NODE_LOOKUP_H
