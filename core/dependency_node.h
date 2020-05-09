#ifndef ICARUS_CORE_DEPENDENCY_NODE_H
#define ICARUS_CORE_DEPENDENCY_NODE_H

#include <utility>

namespace core {

enum class DependencyNodeKind {
  ParamType  = 0,
  ParamValue = 1,
  ArgType    = 2,
  ArgValue   = 3
};

template <typename T>
struct DependencyNode {
  using node_type = T;
  static_assert(alignof(node_type) >= 4);

  static DependencyNode MakeType(node_type const *decl) {
    return DependencyNode(decl, DependencyNodeKind::ParamType);
  }

  static DependencyNode MakeValue(node_type const *decl) {
    return DependencyNode(decl, DependencyNodeKind::ParamValue);
  }

  static DependencyNode MakeArgType(node_type const *decl) {
    return DependencyNode(decl, DependencyNodeKind::ArgType);
  }

  static DependencyNode MakeArgValue(node_type const *decl) {
    return DependencyNode(decl, DependencyNodeKind::ArgValue);
  }

  template <typename H>
  friend H AbslHashValue(H h, DependencyNode n) {
    return H::combine(std::move(h), n.node_);
  }

  friend bool operator==(DependencyNode lhs, DependencyNode rhs) {
    return lhs.node_ == rhs.node_;
  }

  friend bool operator!=(DependencyNode lhs, DependencyNode rhs) {
    return not(lhs == rhs);
  }

  node_type const *node() const {
    return reinterpret_cast<node_type const *>(node_ & ~uintptr_t{3});
  }

  constexpr DependencyNodeKind kind() const {
    return static_cast<DependencyNodeKind>(node_ & 3);
  }

 private:
  explicit DependencyNode(node_type const *node, DependencyNodeKind k)
      : node_(reinterpret_cast<uintptr_t>(node) | static_cast<uintptr_t>(k)) {}

  uintptr_t node_;
};

inline constexpr char const *ToString(DependencyNodeKind k) {
  switch (k) {
    case DependencyNodeKind::ParamType: return "param-type";
    case DependencyNodeKind::ParamValue: return "param-value";
    case DependencyNodeKind::ArgType: return "arg-type";
    case DependencyNodeKind::ArgValue: return "arg-value";
  }
}

}  // namespace core

#endif  // ICARUS_CORE_DEPENDENCY_NODE_H
