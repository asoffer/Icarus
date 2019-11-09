#ifndef ICARUS_MODULE_DEPENDENT_DECLS_H
#define ICARUS_MODULE_DEPENDENT_DECLS_H

#include <array>
#include <vector>

#include "ast/ast_fwd.h"
#include "ast/visitor.h"
#include "base/debug.h"
#include "base/graph.h"

namespace module {
struct DeclDepGraph {
  base::Graph<ast::Declaration const *> graph_;

  // Some declarations may depend on identifiers, but it could be expensive to
  // compute which declaration that identifier refers to. We may also not have
  // verified everything about that declaration or determined it's type to know
  // which overloads we need. Arbitrarily complicated computation may be needed
  // here. However, lucky for us, we don't care about all declarations, just the
  // ones inside the parameter list. So what we do is check the identifier token
  // and match it to a declaration in this list if there is one, greatly
  // simplifying the required task.
  //
  // This may lead to problems with function overloading though if functions
  // visible in this scope have the same name as a parameter (which would also
  // have to be a function). The problem is that we would pick up the function
  // parameter even if we intended the overload that wasn't a parameter (which
  // we might know if we bothered to check types). This would create an
  // unintended edge in the graph which could create loops. I think we may just
  // want to disallow this.
  //
  // TODO Either disallow this type of shadowing, or figure out another way to
  // solve this problem.
  absl::flat_hash_map<std::string_view, std::vector<ast::Declaration const *>>
      ids_;
};

struct DependentDecls : ast::Visitor<void(ast::Declaration const *)> {
  void Visit(ast::Node const *node, ast::Declaration const *d) {
    ast::Visitor<void(ast::Declaration const *)>::Visit(node, d);
  }

#define ICARUS_AST_NODE_X(name)                                                \
  void Visit(ast::name const *node, ast::Declaration const *d);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  // TODO make private.
  DeclDepGraph decl_graph_;
};

}  // namespace module

#endif  // ICARUS_MODULE_DEPENDENT_DECLS_H
