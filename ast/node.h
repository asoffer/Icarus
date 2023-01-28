#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <string>

#include "base/cast.h"
#include "base/visitable.h"

namespace ast {
struct Expression;
struct Scope;

inline constexpr auto AllNodeTypes = nth::type_sequence<int
#define ICARUS_AST_NODE_X(node) , struct node
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X
                                                >.tail();

template <typename T>
constexpr ssize_t IndexOf() {
  return AllNodeTypes.find_if<[](auto t) { return t == nth::type<T>; }>();
}

struct Node : base::Visitable<Node, AllNodeTypes>, base::Cast<Node> {
  explicit constexpr Node(int8_t which, std::string_view range = "")
      : base::Visitable<Node, AllNodeTypes>(which), range_(range) {}

  virtual ~Node() {}

  std::string DebugString() const {
    std::string out;
    DebugStrAppend(&out, 0);
    return out;
  }

  virtual void DebugStrAppend(std::string *out, size_t indent) const {}
  bool covers_binding() const { return covers_binding_; }
  bool is_dependent() const { return is_dependent_; }

  constexpr std::string_view range() const { return range_; }
  Scope *scope() const { return scope_; }

  // Object used to track state while initializing the syntax tree.
  struct Initializer {
    Scope *scope = nullptr;

    // The closest parent `FunctionLiteral` node. Used to link up function
    // literals with their corresponding return statements.
    FunctionLiteral *function_literal = nullptr;
    std::vector<ScopeNode *> scope_nodes;

    PatternMatch const *pattern = nullptr;
    Declaration *match_against  = nullptr;
  };

  virtual void Initialize(Initializer &initializer) {}

 protected:
  std::string_view range_;
  Scope *scope_ = nullptr;
  // TODO: We can compress these bit somewhere.
  bool covers_binding_ = false;
  bool is_dependent_   = false;
};

}  // namespace ast

#endif  // ICARUS_AST_NODE_H
