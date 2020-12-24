#include "ast/ast.h"
#include "base/move_func.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "core/arguments.h"
#include "ir/value/value.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::Jump const *node) {
  LOG("Jump", "Emit %s", node->DebugString());
  // TODO: Check the result of body verification.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }

  auto [jmp, inserted] = context().add_jump(node);
  if (inserted) {
    LOG("compile-work-queue", "Request work complete struct: %p", node);
    Enqueue({
        .kind      = WorkItem::Kind::EmitJumpBody,
        .node      = node,
        .resources = resources_,
    });
  }
  return ir::Value(jmp);
}

WorkItem::Result Compiler::EmitJumpBody(ast::Jump const *node) {
  LOG("EmitJumpBody", "Jump %s", node->DebugString());
  ir::CompiledJump &jmp = *ASSERT_NOT_NULL(context().jump(node));

  ICARUS_SCOPE(ir::SetCurrent(jmp, builder())) {
    builder().CurrentBlock() = jmp.entry();
    // TODO arguments should be renumbered to not waste space on const
    // values
    int32_t i = 0;
    if (node->state()) { context().set_addr(node->state(), ir::Reg::Arg(i++)); }
    for (auto const &param : node->params()) {
      context().set_addr(param.value.get(), ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*this, node->body_scope());
    EmitIrForStatements(*this, node->stmts());

    // TODO: it seems like this will be appended after ChooseJump, which means
    // it'll never be executed.
    MakeAllDestructions(*this, node->body_scope());
  }
  jmp.WriteByteCode<interpreter::instruction_set_t>();
  return WorkItem::Result ::Success;
}

}  // namespace compiler
