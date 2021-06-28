#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/builder.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::BlockNode const *node,
                            base::untyped_buffer &) {
  LOG("BlockNode", "EmitValue for block node named %s", node->name());
  EmitIrForStatements(*this, node->stmts());
  MakeAllDestructions(*this, &node->body_scope());
  auto &termination = builder().block_termination_state();
  if (termination == ir::Builder::BlockTerminationState::kMoreStatements) {
    termination = ir::Builder::BlockTerminationState::kNoTerminator;
  }
}

}  // namespace compiler
