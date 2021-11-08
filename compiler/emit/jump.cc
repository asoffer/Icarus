#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/instructions.h"
#include "core/arguments.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::Jump const *node, ir::PartialResultBuffer &out) {
  LOG("Jump", "Emit %s", node->DebugString());
  auto [jmp, inserted] = context().add_jump(node);
  if (inserted) {
    LOG("compile-work-queue", "Request work complete struct: %p", node);
    Enqueue({.kind    = WorkItem::Kind::EmitJumpBody,
             .node    = node,
             .context = &context()});
  }
  out.append(jmp);
}

bool Compiler::EmitJumpBody(ast::Jump const *node) {
  LOG("EmitJumpBody", "Jump %s", node->DebugString());
  ir::CompiledJump &jmp = *ASSERT_NOT_NULL(context().jump(node));

  ICARUS_SCOPE(ir::SetCurrent(jmp, builder())) {
    builder().CurrentBlock() = jmp.entry();
    // TODO arguments should be renumbered to not waste space on const
    // values
    int32_t i = 0;
    if (node->state()) {
      // TODO: Support multiple declarations?
      builder().set_addr(&node->state()->ids()[0], ir::Reg::Arg(i++));
    }
    for (auto const &param : node->params()) {
      // TODO: Support multiple declarations?
      builder().set_addr(&param.value->ids()[0], ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*this, &node->body_scope());
    EmitIrForStatements(*this, node->stmts());

    // TODO: it seems like this will be appended after ChooseJump, which means
    // it'll never be executed.
    MakeAllDestructions(*this, &node->body_scope());
  }

  return true;
}

}  // namespace compiler
