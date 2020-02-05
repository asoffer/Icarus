#include "compiler/dispatch/jump_table.h"

#include "base/debug.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/match.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "core/fn_params.h"
#include "ir/inliner.h"

namespace compiler {

base::expected<JumpDispatchTable> JumpDispatchTable::Verify(
    ast::ScopeNode const *node, absl::Span<ir::Jump *const> jumps,
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

absl::flat_hash_map<
    std::string_view,
    std::pair<ir::BasicBlock *, core::FnArgs<type::Typed<ir::Results>>>>
JumpDispatchTable::EmitCall(
    ir::Jump *jump,
    Compiler *compiler, core::FnArgs<type::Typed<ir::Results>> args,
    ir::LocalBlockInterpretation const &block_interp) {
  // TODO actually choose correctly.
  core::FillMissingArgs(jump->params(), &args, [compiler](auto const &p) {
    return type::Typed(
        ir::Results{compiler->Visit(ASSERT_NOT_NULL(p.get()->init_val()),
                                    EmitValueTag{})},
        p.type());
  });

  auto arg_results = PrepareCallArguments(
      compiler,
      jump->params().Transform([](auto const &p) { return p.type(); }), args);

  return ir::Inline(compiler->builder(), jump, arg_results, block_interp);
}

}  // namespace compiler
