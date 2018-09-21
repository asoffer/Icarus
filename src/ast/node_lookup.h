#ifndef ICARUS_AST_NODE_LOOKUP_H
#define ICARUS_AST_NODE_LOOKUP_H

#include "base/container/unordered_map.h"
#include "base/container/vector.h"

namespace AST {
struct Node;
struct Identifier;

template <typename T> struct NodeLookup {
  template <typename... Args>
  auto emplace(Args&&... args) {
    return data_.emplace(std::forward<Args>(args)...);
  }

  template <typename... Args>
  auto buffered_emplace(Args&&... args) {
    return buffer_.emplace_back(std::forward<Args>(args)...);
  }

  base::unordered_map<Node*, T> data_;
  base::vector<std::pair<Node*, T>> buffer_;
};
}  // namespace AST

#endif  // ICARUS_AST_NODE_LOOKUP_H
