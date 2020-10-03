#include "compiler/dispatch/jump_table.h"

#include "base/debug.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/match.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "core/params.h"
#include "ir/inliner.h"
#include "ir/value/jump.h"

namespace compiler {

base::expected<JumpDispatchTable> JumpDispatchTable::Verify(
    type::Type const *state_type, absl::flat_hash_set<ir::Jump> const &jumps,
    core::FnArgs<type::QualType> const &args) {
  LOG("dispatch-verify", "Verifying overload set with %u members.",
      jumps.size());

  // Keep a collection of failed matches around so we can give better
  // diagnostics.
  absl::flat_hash_map<ir::Jump, FailedMatch> failures;
  JumpDispatchTable table;
  for (ir::Jump jump : jumps) {
    // TODO the type of the specific overload could *correctly* be null and
    // we need to handle that case.
    LOG("dispatch-verify", "Verifying %s", jump);

    auto result = MatchArgsToParams(
        ir::CompiledJump::From(jump)->params().Transform([](auto const &p) {
          return type::QualType::NonConstant(p.type());
        }),
        args);
    if (not result) {
      failures.emplace(jump, result.error());
    } else {
      table.table_.emplace(
          std::piecewise_construct, std::forward_as_tuple(jump),
          std::forward_as_tuple(ir::CompiledJump::From(jump)->type(), *result));
    }
  }

  if (not ParamsCoverArgs(args, table.table_,
                          [](auto const &, internal::ExprData const &data) {
                            return data.params();
                          })) {
    LOG("", "%s", args.to_string());
    NOT_YET("log an error");
  }
  return table;
}

absl::flat_hash_map<
    std::string_view,
    std::pair<ir::BasicBlock *, core::FnArgs<type::Typed<ir::Value>>>>
JumpDispatchTable::EmitCallOneOverload(
    std::optional<ir::Reg> state_reg, ir::Jump jump, Compiler *compiler,
    core::FnArgs<type::Typed<ir::Value>> args,
    ir::LocalBlockInterpretation const &block_interp) {
  // TODO actually choose correctly.
  if (state_reg) {
    auto pos   = std::move(args).pos();
    auto named = std::move(args).named();
    pos.insert(pos.begin(), type::Typed<ir::Value>(
                                ir::Value(*state_reg),
                                ir::CompiledJump::From(jump)->type()->state()));
    args = core::FnArgs(std::move(pos), std::move(named));
  }
  core::FillMissingArgs(core::ParamsRef(ir::CompiledJump::From(jump)->params()),
                        &args,
                        [](auto const &p) -> type::Typed<ir::Value> {
                          NOT_YET();
                          // return type::Typed(
                          //     ir::Results{compiler->EmitValue(
                          //         ASSERT_NOT_NULL(p.get()->init_val()))},
                          //     p.type());
                        },
                        state_reg ? 1 : 0);

  // TODO qualtype? non constant?
  auto arg_values = PrepareCallArguments(
      compiler, ir::CompiledJump::From(jump)->type()->state(),
      ir::CompiledJump::From(jump)->params().Transform(
          [](auto const &p) { return type::QualType::NonConstant(p.type()); }),
      args);
  return ir::Inline(compiler->builder(), jump, arg_values, block_interp);
}

}  // namespace compiler
