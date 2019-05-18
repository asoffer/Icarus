#ifndef ICARUS_VISITOR_ASSIGN_SCOPE_H
#define ICARUS_VISITOR_ASSIGN_SCOPE_H

#include "core/scope.h"
#include "ast/ast_fwd.h"

namespace visitor {

struct AssignScope {
#define ICARUS_AST_NODE_X(name)                                                \
  void operator()(ast::name *node, core::Scope *scope);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X
};

}  // namespace visitor

#endif  // ICARUS_VISITOR_ASSIGN_SCOPE_H
