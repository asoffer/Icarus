#ifndef ICARUS_VISITOR_MATCH_H
#define ICARUS_VISITOR_MATCH_H

#include "ast/ast_fwd.h"
#include "base/debug.h"

namespace match {
struct BindingNode;
}  // namespace match

namespace visitor {

struct Match {
  void MatchExpr(ast::Node const *node, ast::Expression const *pattern,
                 bool allow_submatch);
#define ICARUS_AST_NODE_X(name)                                                \
  void MatchExpr(ast::name const *node, ast::Expression const *pattern,        \
                 bool allow_submatch);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X
};

}  // namespace visitor

#endif  // ICARUS_VISITOR_MATCH_H
