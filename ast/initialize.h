#ifndef ICARUS_AST_INITIALIZE_H
#define ICARUS_AST_INITIALIZE_H

#include "ast/ast_fwd.h"
#include "ast/scope/scope.h"
#include "ast/visitor.h"
#include "base/debug.h"

namespace ast {

struct Initialize : MutableVisitor<void(Scope *scope)> {
  ~Initialize() override {}
  void Visit(Node *node, Scope *scope) {
    MutableVisitor<void(Scope *)>::Visit(node, scope);
  }

#define ICARUS_AST_NODE_X(name) void Visit(name *node, Scope *scope) override;
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X
};

void InitializeNodes(base::PtrSpan<Node> nodes, Scope *scope);

}  // namespace ast

#endif  // ICARUS_AST_INITIALIZE_H
