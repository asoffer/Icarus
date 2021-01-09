#ifndef ICARUS_AST_NODE_H
#define ICARUS_AST_NODE_H

#include <utility>

#include "ast/visitor_base.h"
#include "base/cast.h"
#include "frontend/source/buffer.h"

namespace ast {
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
  virtual void Initialize(Scope *scope) {}
  virtual bool IsDependent() const { return false; }

  constexpr frontend::SourceRange range() const { return range_; }
  Scope *scope() const { return scope_; }

  frontend::SourceRange range_;
  Scope *scope_ = nullptr;
};

}  // namespace ast

#endif  // ICARUS_AST_NODE_H
