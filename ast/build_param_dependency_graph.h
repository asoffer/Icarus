#ifndef ICARUS_AST_BUILD_PARAM_DEPENDENCY_GRAPH_H
#define ICARUS_AST_BUILD_PARAM_DEPENDENCY_GRAPH_H

#include <memory>

#include "ast/ast_fwd.h"
#include "base/graph.h"
#include "core/params.h"

namespace ast {
struct Declaration;

struct DependencyNode {
  enum class Kind { ParamType = 0, ParamValue = 1, ArgType = 2, ArgValue = 3 };

  static DependencyNode MakeType(Declaration const *decl) {
    return DependencyNode(decl, Kind::ParamType);
  }

  static DependencyNode MakeValue(Declaration const *decl) {
    return DependencyNode(decl, Kind::ParamValue);
  }

  static DependencyNode MakeArgType(Declaration const *decl) {
    return DependencyNode(decl, Kind::ArgType);
  }

  static DependencyNode MakeArgValue(Declaration const *decl) {
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
    return !(lhs == rhs);
  }

  Declaration const *decl() const {
    return reinterpret_cast<Declaration const *>(node_ & ~uintptr_t{3});
  }

  constexpr Kind kind() const { return static_cast<Kind>(node_ & 3); }

 private:
  explicit DependencyNode(Declaration const *node, Kind k)
      : node_(reinterpret_cast<uintptr_t>(node) | static_cast<uintptr_t>(k)) {}

  uintptr_t node_;
};

base::Graph<DependencyNode> BuildParamDependencyGraph(
    core::Params<std::unique_ptr<Declaration>> const &params);

}  // namespace ast

#endif  // ICARUS_AST_BUILD_PARAM_DEPENDENCY_GRAPH_H
