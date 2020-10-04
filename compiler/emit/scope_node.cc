#include <optional>
#include <utility>

#include "ast/ast.h"
#include "base/defer.h"
#include "compiler/compiler.h"
#include "ir/compiled_scope.h"
#include "ir/value/reg.h"
#include "ir/value/scope.h"
#include "ir/value/value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::ScopeNode const *node) {
  LOG("ScopeNode", "Emitting IR for ScopeNode");

  ASSIGN_OR(return ir::Value(),  //
                   auto scope, EvaluateOrDiagnoseAs<ir::Scope>(node->name()));

  // Stateful scopes need to have their state initialized.
  std::optional<ir::Reg> state_ptr;
  if (auto const *state_type = ir::CompiledScope::From(scope)->state_type()) {
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
  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed<ir::Value>(EmitValue(expr), type_of(expr));
  });

  // TODO: Implement me.
  // auto const &inits = scope_def->start_->after();
  // if (inits.size() != 1u) { NOT_YET("Suuport overload sets here."); }
  // auto const &init = *inits.begin();

  builder().UncondJump(landing_block);
  builder().CurrentBlock() = landing_block;
  return ir::Value();
}

}  // namespace compiler
