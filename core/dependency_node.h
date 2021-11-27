#ifndef ICARUS_CORE_DEPENDENCY_NODE_H
#define ICARUS_CORE_DEPENDENCY_NODE_H

#include <ostream>
#include <utility>

#include "base/extend.h"
#include "base/extend/absl_hash.h"

namespace core {

enum class DependencyNodeKind {
  ParameterType  = 0,
  ParameterValue = 1,
  ArgumentType   = 2,
  ArgumentValue  = 3
};

template <typename T>
struct DependencyNode
    : base::Extend<DependencyNode<T>,
                   1>::template With<base::AbslHashExtension> {
  using node_type = T;
  static_assert(alignof(node_type) >= 4);

  explicit DependencyNode(node_type const *node, DependencyNodeKind k)
      : node_(reinterpret_cast<uintptr_t>(node) | static_cast<uintptr_t>(k)) {}

  static DependencyNode ParameterType(node_type const *decl) {
    return DependencyNode(decl, DependencyNodeKind::ParameterType);
  }

  static DependencyNode ParameterValue(node_type const *decl) {
    return DependencyNode(decl, DependencyNodeKind::ParameterValue);
  }

  static DependencyNode ArgumentType(node_type const *decl) {
    return DependencyNode(decl, DependencyNodeKind::ArgumentType);
  }

  static DependencyNode ArgumentValue(node_type const *decl) {
    return DependencyNode(decl, DependencyNodeKind::ArgumentValue);
  }

  node_type const *node() const {
    return reinterpret_cast<node_type const *>(node_ & ~uintptr_t{3});
  }

  constexpr DependencyNodeKind kind() const {
    return static_cast<DependencyNodeKind>(node_ & 3);
  }

  friend std::ostream &operator<<(std::ostream &os, DependencyNode n) {
    static constexpr std::array<char const *, 4> prefixes = {
        "param-type-", "param-value-", "arg-type-", "arg-value-"};
    return os << prefixes[static_cast<int>(n.kind())] << n.node();
  }

 private:
  friend base::EnableExtensions;
  uintptr_t node_;
};

}  // namespace core

#endif  // ICARUS_CORE_DEPENDENCY_NODE_H
