#include <optional>
#include <utility>

#include "absl/strings/str_format.h"
#include "ast/ast.h"
#include "base/defer.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "ir/compiled_scope.h"
#include "ir/inliner.h"
#include "ir/value/reg.h"
#include "ir/value/scope.h"
#include "ir/value/value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::ScopeNode const *node) {
  LOG("ScopeNode", "Emitting IR for ScopeNode");

  ASSIGN_OR(return ir::Value(),  //
                   auto scope, EvaluateOrDiagnoseAs<ir::Scope>(node->name()));
  auto const *compiled_scope = ir::CompiledScope::From(scope);
  // Stateful scopes need to have their state initialized.
  std::optional<ir::Reg> state_ptr;
  if (auto const *state_type = compiled_scope->state_type()) {
    state_ptr = builder().Alloca(state_type);
  }

  // Arguments to the scope's start must be re-evaluated on each call to `goto
  // start()`, so weneed  a block to which we can jump for this purpose.
  auto *args_block =
      builder().AddBlock(absl::StrFormat("args block for scope %p", node));
  auto *landing_block =
      builder().AddBlock(absl::StrFormat("landing block for scope %p", node));

  // TODO: Support blocks evaluating to values.
  add_scope_landing(TransientState::ScopeLandingState{
      .label = node->label() ? node->label()->value() : ir::Label(),
      .block = landing_block,
      .phi   = nullptr,
  });
  base::defer d = [&] { pop_scope_landing(); };

  auto local_interp =
      builder().MakeLocalBlockInterpretation(node, args_block, landing_block);

  // Evaluate the arguments on the initial `args_block`.
  builder().UncondJump(args_block);
  builder().CurrentBlock() = args_block;

  // TODO: Support dynamic dispatch.
  auto const &inits = compiled_scope->inits();
  ASSERT(inits.size() == 1u);
  auto &init = *inits.begin();

  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed<ir::Value>(EmitValue(expr), type_of(expr));
  });

  auto arg_values = PrepareCallArguments(
      this, ir::CompiledJump::From(init)->type()->state(),
      ir::CompiledJump::From(init)->params().Transform(
          [](auto const &p) { return type::QualType::NonConstant(p.type()); }),
      args);

  auto block_map = ir::Inline(builder(), init, arg_values, local_interp);

  for (auto const &block_node : node->blocks()) {
    auto const &[block, args] = block_map.at(block_node.name());
    builder().CurrentBlock()  = block;

    auto *start = local_interp[block_node.name()];
    builder().UncondJump(start);

    builder().CurrentBlock()  = start;
    auto *b                   = builder().AddBlock(
        absl::StrFormat("body block for `%s`.", block_node.name()));
    builder().UncondJump(b);

    builder().CurrentBlock() = b;
    EmitValue(&block_node);

    // TODO: Get yielded arguments.
    auto const *scope_block =
        ir::CompiledBlock::From(compiled_scope->block(block_node.name()));

    auto const &afters = scope_block->after();
    // TODO: Choose the right jump.
    ASSERT(afters.size() == 1u);
    auto &after = *afters.begin();
    auto landing_block_map = ir::Inline(builder(), after, {}, local_interp);
    // TODO: This is a hack/wrong
    for (const auto &[name, block_and_args] : landing_block_map) {
      builder().CurrentBlock() = block_and_args.first;
      builder().UncondJump(local_interp[name]);
    }
  }

  // TODO: Support arguments to `done()`. Need to bind these to local variables.
  auto const &[done_block, done_args] = block_map.at("done");
  builder().CurrentBlock()            = done_block;
  builder().UncondJump(landing_block);

  builder().CurrentBlock() = landing_block;
  return ir::Value();
}

}  // namespace compiler
