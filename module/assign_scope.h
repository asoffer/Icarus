#ifndef ICARUS_MODULE_ASSIGN_SCOPE_H
#define ICARUS_MODULE_ASSIGN_SCOPE_H

#include "ast/ast_fwd.h"
#include "ast/scope/scope.h"
#include "ast/visitor.h"
#include "base/debug.h"

namespace module {

struct AssignScope : ast::MutableVisitor<void(ast::Scope *scope)> {
  void Visit(ast::Node *node, ast::Scope *scope) {
    ast::MutableVisitor<void(ast::Scope *)>::Visit(node, scope);
  }

#define ICARUS_AST_NODE_X(name)                                                \
  void Visit(ast::name *node, ast::Scope *scope);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  static void To(base::PtrSpan<ast::Node> nodes, ast::Scope *scope);
};

}  // namespace module

#endif  // ICARUS_MODULE_ASSIGN_SCOPE_H
