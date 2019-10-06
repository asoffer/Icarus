#ifndef ICARUS_MATCH_MATCH_EXPR_H
#define ICARUS_MATCH_MATCH_EXPR_H

#include <queue>

#include "ast/ast_fwd.h"
#include "base/debug.h"

namespace match {
struct BindingNode;

struct Match {
  struct State {
    State() = delete;
    explicit State(ast::Node const *node, ast::Expression const *pattern)
        : root_(node), current_node_(node), current_pattern_(pattern) {}

    ast::Node const *root_                  = nullptr;
    ast::Node const *current_node_          = nullptr;
    ast::Expression const *current_pattern_ = nullptr;
    // TODO cascading failure? if this node fails to match it should trigger a
    // failure in a further queue-state? Cascading success?
  };

  void MatchAll(ast::Node const *node, ast::Expression const *pattern);

  void MatchExpr(ast::Node const *node, Match::State *state);
#define ICARUS_AST_NODE_X(name)                                                \
  void MatchExpr(ast::name const *node, Match::State *state);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

 private:
  std::queue<State> states_;
};

}  // namespace match

#endif  // ICARUS_MATCH_MATCH_EXPR_H
