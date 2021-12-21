#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/builder.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::BlockNode const *node,
                            ir::PartialResultBuffer &) {
  LOG("BlockNode", "EmitToBuffer for block node named %s", node->name());

  builder().block_termination_state() =
      ir::Builder::BlockTerminationState::kMoreStatements;

  EmitIrForStatements(*this, node->stmts());
  MakeAllDestructions(*this, &node->body_scope());
  // TODO: Probably just a yield jump.
}

}  // namespace compiler
