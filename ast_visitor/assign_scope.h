#ifndef ICARUS_AST_VISITOR_ASSIGN_SCOPE_H
#define ICARUS_AST_VISITOR_ASSIGN_SCOPE_H

#include "core/scope.h"
#include "ast/ast_fwd.h"

namespace ast_visitor {

struct AssignScope {
#define ICARUS_AST_NODE_X(name)                                                \
  void operator()(ast::name *node, core::Scope *scope);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X
};

}  // namespace ast_visitor

#endif  // ICARUS_AST_VISITOR_ASSIGN_SCOPE_H
