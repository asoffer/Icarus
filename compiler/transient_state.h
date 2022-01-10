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

  ir::RegOr<ir::addr_t> addr(ast::Declaration::Id const *id) const {
    return addr_.at(id);
  }
  void set_addr(ast::Declaration::Id const *id, ir::RegOr<ir::addr_t> addr) {
    addr_.emplace(id, addr);
  }

  ir::Reg TmpAlloca(type::Type t) {
    auto reg = builder.Alloca(t);
    temporaries_to_destroy.emplace_back(reg, t);
    return reg;
  }

  // Temporaries need to be destroyed at the end of each statement.  This is a
  // pointer to a buffer where temporary allocations can register themselves for
  // deletion.
  std::vector<type::Typed<ir::Reg>> temporaries_to_destroy;

 private:
  // Stores addresses of local identifiers
  absl::flat_hash_map<ast::Declaration::Id const *, ir::RegOr<ir::addr_t>>
      addr_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_TRANSIENT_STATE_H
