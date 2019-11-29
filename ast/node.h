#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <utility>

#include "ast/visitor_base.h"
#include "base/cast.h"
#include "frontend/source/range.h"

namespace ast {
struct Declaration;
struct Scope;

struct Node : public base::Cast<Node> {
  Node(frontend::SourceRange span = frontend::SourceRange())
      : span(std::move(span)) {}
  virtual ~Node() {}

  virtual void Accept(MutableVisitorBase *visitor, void *ret,
                      void *arg_tuple)       = 0;
  virtual void Accept(VisitorBase *visitor, void *ret,
                      void *arg_tuple) const = 0;

  std::string DebugString() const {
    std::string out;
    DebugStrAppend(&out, 0);
    return out;
  }

  virtual void DebugStrAppend(std::string *out, size_t indent) const {};

  ast::Scope *scope_ = nullptr;
  frontend::SourceRange span;
};

}  // namespace ast

#endif  // ICARUS_AST_NODE_H
