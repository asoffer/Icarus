#include "compiler/dispatch/jump_table.h"

#include "base/debug.h"
#include "compiler/dispatch/match.h"
#include "parameters_and_arguments.h"

namespace compiler {
base::expected<JumpDispatchTable> JumpDispatchTable::Verify(
    Compiler *compiler, ast::ScopeNode const *node,
    absl::Span<ir::Jump *const> jumps,
    core::FnArgs<type::QualType> const &args) {
  DEBUG_LOG("dispatch-verify")
  ("Verifying overload set with ", jumps.size(), " members.");

  // Keep a collection of failed matches around so we can give better
  // diagnostics.
  absl::flat_hash_map<ir::Jump *, FailedMatch> failures;
  JumpDispatchTable table;
  for (ir::Jump *jump : jumps) {
    // TODO the type of the specific overload could *correctly* be null and
    // we need to handle that case.
    DEBUG_LOG("dispatch-verify")("Verifying ", jump);
    auto result = MatchArgsToParams(jump->params(), args);
    if (not result) {
      failures.emplace(jump, result.error());
    } else {
      // TODO you also call compiler->type_of inside ExtractParams, so it's
      // probably worth reducing the number of lookups.
      table.table_.emplace(std::piecewise_construct,
                           std::forward_as_tuple(jump),
                           std::forward_as_tuple(jump->type(), *result));
    }
  }

  if (not ParamsCoverArgs(args, table.table_,
                          [](auto const &, internal::ExprData const &data)
                              -> decltype(auto) { return data.params(); })) {
    NOT_YET("log an error");
  }
  return table;
}
}  // namespace compiler
