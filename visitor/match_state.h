#ifndef ICARUS_VISITOR_MATCH_STATE_H
#define ICARUS_VISITOR_MATCH_STATE_H

#include "ast/ast_fwd.h"

namespace visitor {
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
}  // namespace visitor

#endif  // ICARUS_VISITOR_MATCH_STATE_H
