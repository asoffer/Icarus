#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/compiler_common.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::BlockNode const *node,
                            ir::PartialResultBuffer &) {
  LOG("BlockNode", "EmitToBuffer for block node named %s", node->name());

  for (auto const &param : node->parameters()) {
    auto addr = current().subroutine->Alloca(
        context().qual_types(&param.value)[0].type());
    // TODO: Support multiple declarations?
    state().set_addr(&param.value.ids()[0], addr);
  }

  EmitIrForStatements(*this, &node->body_scope(), node->stmts());
}

}  // namespace compiler
