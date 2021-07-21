#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/builder.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::BlockNode const *node,
                            ir::PartialResultBuffer &) {
  LOG("BlockNode", "EmitToBuffer for block node named %s", node->name());
  EmitIrForStatements(*this, node->stmts());
  MakeAllDestructions(*this, &node->body_scope());
  auto &termination = builder().block_termination_state();
  if (termination == ir::Builder::BlockTerminationState::kMoreStatements) {
    termination = ir::Builder::BlockTerminationState::kNoTerminator;
    auto &scope_state = state().scope_landings.back();
    auto &jumps =
        ir::CompiledBlock::From(
            ir::CompiledScope::From(scope_state.scope)->block(node->name()))
            ->after();
    // TODO: Do overload lookup.
    ASSERT(jumps.size() == 1u);
    ir::Jump j = *jumps.begin();
    ir::PartialResultBuffer args;
    builder().InlineJumpIntoCurrent(j, args,
                                    state().scope_landings.back().names);
  }
}

}  // namespace compiler
