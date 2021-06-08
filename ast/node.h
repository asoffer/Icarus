#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <utility>

#include "ast/visitor_base.h"
#include "base/cast.h"
#include "frontend/source/buffer.h"

namespace ast {
struct FunctionLiteral;
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
  virtual bool IsDependent() const { return false; }

  constexpr frontend::SourceRange range() const { return range_; }
  Scope *scope() const { return scope_; }

  // Object used to track state while initializing the syntax tree.
  struct Initializer {
    Scope *scope = nullptr;

    // The closest parent `FunctionLiteral` node. Used so that `ReturnStmt`
    // nodes can point back to their corresponding function literal from which
    // they return.
    FunctionLiteral const *function_literal = nullptr;
  };

  virtual void Initialize(Initializer &initializer) {}

 protected:
  frontend::SourceRange range_;
  Scope *scope_ = nullptr;
};

}  // namespace ast

#endif  // ICARUS_AST_NODE_H
