#ifndef ICARUS_CORE_DEPENDENCY_NODE_H
#define ICARUS_CORE_DEPENDENCY_NODE_H

#include <utility>

namespace core {

template <typename T>
struct DependencyNode {
  using node_type = T;
  static_assert(alignof(node_type) >= 4);

  enum class Kind { ParamType = 0, ParamValue = 1, ArgType = 2, ArgValue = 3 };

  static DependencyNode MakeType(node_type const *decl) {
    return DependencyNode(decl, Kind::ParamType);
  }

  static DependencyNode MakeValue(node_type const *decl) {
    return DependencyNode(decl, Kind::ParamValue);
  }

  static DependencyNode MakeArgType(node_type const *decl) {
    return DependencyNode(decl, Kind::ArgType);
  }

  static DependencyNode MakeArgValue(node_type const *decl) {
    return DependencyNode(decl, Kind::ArgValue);
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

  constexpr Kind kind() const { return static_cast<Kind>(node_ & 3); }

 private:
  explicit DependencyNode(node_type const *node, Kind k)
      : node_(reinterpret_cast<uintptr_t>(node) | static_cast<uintptr_t>(k)) {}

  uintptr_t node_;
};

}  // namespace core

#endif // ICARUS_CORE_DEPENDENCY_NODE_H
