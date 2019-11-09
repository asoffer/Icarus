#ifndef ICARUS_AST_VISITOR_BASE_H
#define ICARUS_AST_VISITOR_BASE_H

#include "ast/ast_fwd.h"
#include "base/debug.h"
#include "base/meta.h"

namespace ast {

struct VisitorBase {
  virtual ~VisitorBase() {}

  virtual void ErasedVisit(Node const*, void *, void *) {
    UNREACHABLE();
  }

#define ICARUS_AST_NODE_X(node_type)                                           \
  virtual void ErasedVisit(node_type const *, void *, void *) = 0;
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X
};

struct MutableVisitorBase {
  virtual ~MutableVisitorBase() {}

  virtual void ErasedVisit(Node *, void *, void *) { UNREACHABLE(); }

#define ICARUS_AST_NODE_X(node_type)                                           \
  virtual void ErasedVisit(node_type *, void *, void *) = 0;
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X
};

}  // namespace ast

#endif  // ICARUS_AST_VISITOR_BASE_H
