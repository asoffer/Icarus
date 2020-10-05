#include <optional>
#include <utility>

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
  auto *args_block    = builder().AddBlock();
  auto *landing_block = builder().AddBlock();

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

  ir::Inline(builder(), init, arg_values, local_interp);

  builder().CurrentBlock() = landing_block;
  return ir::Value();
}

}  // namespace compiler
