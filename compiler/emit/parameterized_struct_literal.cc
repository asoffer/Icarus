#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/compiler.h"
#include "compiler/emit/struct_literal.h"
#include "compiler/instructions.h"
#include "compiler/struct.h"
#include "type/struct.h"
#include "type/type.h"

namespace compiler {

// TODO: Remove forward declaration.
absl::Span<type::QualType const> VerifyType(CompilationDataReference data,
                                            ast::Node const *node);

void Compiler::EmitToBuffer(ast::ParameterizedStructLiteral const *node,
                            ir::PartialResultBuffer &out) {
  LOG("ParameterizedStructLiteral", "Emitting function for %s",
      node->DebugString());
  auto [f, inserted] = context().add_func(node);
  if (inserted) {
    Enqueue({.kind    = WorkItem::Kind::EmitParameterizedStructFunction,
             .node    = node,
             .context = &context()});
  }

  out.append(ir::Fn(f));
}

bool Compiler::CompleteStruct(ast::ParameterizedStructLiteral const *node) {
  LOG("ParameterizedStructLiteral", "Completing struct-literal emission: %p",
      node);

  // TODO: Find a way around these const casts.
  type::Struct *s =
      &const_cast<type::Struct &>(context().LoadType(node).as<type::Struct>());
  if (s->completeness() == type::Completeness::Complete) {
    LOG("ParameterizedStructLiteral", "Already complete, exiting: %p", node);
    return true;
  }

  ASSIGN_OR(return false,  //
                   auto fn, StructCompletionFn(*this, s, node->fields()));
  // TODO: What if execution fails.
  InterpretAtCompileTime(shared_context(), fn);
  s->complete();
  LOG("struct", "Completed %s which is a struct %s with %u field(s).",
      node->DebugString(), *s, s->fields().size());
  return true;
}

bool Compiler::EmitParameterizedStructFunctionBody(
    ast::ParameterizedStructLiteral const *node) {
  LOG("ParameterizedStructLiteral", "%s", node->DebugString());
  for (auto const &field_decl : node->fields()) {
    if (not(field_decl.flags() & ast::Declaration::f_IsConst)) { continue; }
    VerifyType(*this, &field_decl);
  }

  ir::Fn f = context().FindNativeFn(node);
  auto &subroutine =
      const_cast<ir::Subroutine &>(*shared_context().Function(f).subroutine);
  push_current(&subroutine);

  absl::Cleanup c = [&] { state().current.pop_back(); };

  // TODO: We don't want to allocate each node separately: We need to cache them
  // and look up the values.
  // TODO: Check for copyable/movable.
  // TODO: Set the module appropriately.
  ir::Reg r = current_block()->Append(type::AllocateStructInstruction{
      .mod = ModuleFor(node), .result = current().subroutine->Reserve()});
  EmitStructDataCompletion(*this, r, node->fields());
  current_block()->set_jump(ir::JumpCmd::Return());

  current_block()->Append(ir::SetReturnInstruction<type::Type>{
      .index = 0,
      .value = r,
  });
  current_block()->set_jump(ir::JumpCmd::Return());

  LOG("ParameterizedStructLiteral", "%s", *current().subroutine);
  context().ir().WriteByteCode<EmitByteCode>(f);
  return true;
}

}  // namespace compiler
