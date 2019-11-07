#ifndef ICARUS_MATCH_MATCH_EXPR_H
#define ICARUS_MATCH_MATCH_EXPR_H

#include <queue>

#include "ast/ast_fwd.h"
#include "ast/visitor.h"
#include "base/debug.h"

namespace match {
struct BindingNode;

struct MatchState {
  MatchState() = delete;
  explicit MatchState(ast::Node const *node, ast::Expression const *pattern)
      : root_(node), current_node_(node), current_pattern_(pattern) {}

  ast::Node const *root_                  = nullptr;
  ast::Node const *current_node_          = nullptr;
  ast::Expression const *current_pattern_ = nullptr;
  // TODO cascading failure? if this node fails to match it should trigger a
  // failure in a further queue-state? Cascading success?
};

struct Match : ast::Visitor<void(MatchState *)> {

  void MatchAll(ast::Node const *node, ast::Expression const *pattern);

  void Visit(ast::Node const *node, MatchState *state);
#define ICARUS_AST_NODE_X(name)                                                \
  void Visit(ast::name const *node, MatchState *state);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

 private:
  std::queue<MatchState> states_;
};

}  // namespace match

#endif  // ICARUS_MATCH_MATCH_EXPR_H
