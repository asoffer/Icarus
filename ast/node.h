#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <string>

#include "ast/visitor_base.h"
#include "base/cast.h"
#include "base/meta.h"
#include "frontend/source/buffer.h"

namespace ast {
struct Expression;
struct Scope;

using AllNodeTypes = base::type_list<
#define ICARUS_AST_NODE_X(node) struct node,
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X
    struct EmptyNodeTag>;

namespace internal_node {

template <typename T>
constexpr ssize_t Index() {
  return base::Index<T>(AllNodeTypes{});
}

}  // namespace internal_node

struct Node : base::Cast<Node> {
  explicit constexpr Node(int8_t which,
                          frontend::SourceRange const &range = {})
      : range_(range), which_(which) {}

  virtual ~Node() {}

  virtual void Accept(VisitorBase *visitor, void *ret,
                      void *arg_tuple) const = 0;

  std::string DebugString() const {
    std::string out;
    DebugStrAppend(&out, 0);
    return out;
  }

  virtual void DebugStrAppend(std::string *out, size_t indent) const {}
  bool covers_binding() const { return covers_binding_; }
  bool is_dependent() const { return is_dependent_; }

  int8_t which() const { return which_; }

  constexpr frontend::SourceRange range() const { return range_; }
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
  frontend::SourceRange range_;
  Scope *scope_ = nullptr;
  // TODO: We can compress these bit somewhere.
  bool covers_binding_ = false;
  bool is_dependent_   = false;
  int8_t which_;
};

}  // namespace ast

#endif  // ICARUS_AST_NODE_H
