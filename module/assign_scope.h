#ifndef ICARUS_MODULE_ASSIGN_SCOPE_H
#define ICARUS_MODULE_ASSIGN_SCOPE_H

#include "ast/ast_fwd.h"
#include "ast/scope/scope.h"
#include "base/debug.h"

namespace module {

struct AssignScope {
  void operator()(ast::Node *node, ast::Scope *scope) { UNREACHABLE(); }
#define ICARUS_AST_NODE_X(name)                                                \
  void operator()(ast::name *node, ast::Scope *scope);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X
};

}  // namespace module

#endif  // ICARUS_MODULE_ASSIGN_SCOPE_H
