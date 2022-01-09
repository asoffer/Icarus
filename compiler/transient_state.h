#ifndef ICARUS_COMPILER_TRANSIENT_STATE_H
#define ICARUS_COMPILER_TRANSIENT_STATE_H

#include <queue>
#include <utility>
#include <vector>

#include "ast/node.h"
#include "compiler/cyclic_dependency_tracker.h"
#include "ir/scope_state.h"
#include "ir/value/result_buffer.h"
#include "ir/value/scope.h"
#include "type/type.h"

namespace compiler {

struct PatternMatchingContext {
  type::Type type;
  ir::CompleteResultBuffer value;
  size_t array_type_index = 0;
};

// Compiler state that needs to be tracked during a single depth traversal, but
// that does not otherwise need to be saved.
struct TransientState {
  std::vector<ir::ScopeState> scope_landings;
  std::vector<ir::Scope> scopes;

  void EnqueueVerifyPatternMatchType(ast::Node const *node,
                                     type::Type match_type) {
    verify_pattern_type_queues.back().emplace(node, match_type);
  }

  std::vector<std::queue<std::pair<ast::Node const *, type::Type>>>
      verify_pattern_type_queues;

  void EnqueuePatternMatch(ast::Node const *node,
                           PatternMatchingContext context) {
    pattern_match_queues.back().emplace(node, std::move(context));
  }

  std::vector<std::queue<std::pair<ast::Node const *, PatternMatchingContext>>>
      pattern_match_queues;

  CyclicDependencyTracker cyclic_dependency_tracker;

  IrBuilder builder;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_TRANSIENT_STATE_H
