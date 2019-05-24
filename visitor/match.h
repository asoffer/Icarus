#ifndef ICARUS_VISITOR_MATCH_H
#define ICARUS_VISITOR_MATCH_H

#include <queue>

#include "ast/ast_fwd.h"
#include "base/debug.h"

namespace match {
struct BindingNode;
}  // namespace match

namespace visitor {
struct MatchState;

struct Match {
  Match();
  ~Match();
  void MatchAll(ast::Node const *node, ast::Expression const *pattern);

  void MatchExpr(ast::Node const *node, MatchState *state);
#define ICARUS_AST_NODE_X(name)                                                \
  void MatchExpr(ast::name const *node, MatchState *state);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

 private:
  // TODO this really doesn't need to be a unique_ptr, but it's a fast way to
  // have this particular header not depend on `MatchState`. This is silly
  // long-term, but during development this will greatly cut-down on the rebuild
  // time.
  std::unique_ptr<std::queue<MatchState>> states_;
};

}  // namespace visitor

#endif  // ICARUS_VISITOR_MATCH_H
