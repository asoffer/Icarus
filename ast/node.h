#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <utility>

#include "ast/visitor_base.h"
#include "base/cast.h"
#include "frontend/source/range.h"

namespace ast {
struct Scope;

struct Node : public base::Cast<Node> {
  Node(frontend::SourceRange span = frontend::SourceRange())
      : span(std::move(span)) {}
  virtual ~Node() {}

  virtual void Accept(VisitorBase *visitor, void *ret,
                      void *arg_tuple) const = 0;

  std::string DebugString() const {
    std::string out;
    DebugStrAppend(&out, 0);
    return out;
  }

  virtual void DebugStrAppend(std::string *out, size_t indent) const {}
  virtual void Initialize(Scope *scope) {}

  Scope *scope() const { return scope_; }

  // TODO make private
  frontend::SourceRange span;

 protected:
  Scope *scope_ = nullptr;
};

}  // namespace ast

#endif  // ICARUS_AST_NODE_H
