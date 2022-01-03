#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <string>

#include "ast/visitor_base.h"
#include "base/cast.h"
#include "frontend/source/buffer.h"

namespace ast {
struct Expression;
struct FunctionLiteral;
struct PatternMatch;
struct Scope;

struct Node : base::Cast<Node> {
  explicit constexpr Node(frontend::SourceRange const &range = {})
      : range_(range) {}

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
};

}  // namespace ast

#endif  // ICARUS_AST_NODE_H
