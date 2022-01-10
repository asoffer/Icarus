#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/ir_builder.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::BlockNode const *node,
                            ir::PartialResultBuffer &) {
  LOG("BlockNode", "EmitToBuffer for block node named %s", node->name());

  for (auto const &param : node->params()) {
    auto addr =
        builder().Alloca(context().qual_types(param.value.get())[0].type());
    // TODO: Support multiple declarations?
    state().set_addr(&param.value->ids()[0], addr);
  }

  EmitIrForStatements(*this, node->stmts());
  MakeAllDestructions(*this, &node->body_scope());
  // TODO: Probably just a yield jump.
}

}  // namespace compiler
