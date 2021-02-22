#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/instructions.h"
#include "ir/value/value.h"
#include "type/struct.h"
#include "type/type.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::ParameterizedStructLiteral const *node) {
  // TODO: Check the result of body verification.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }
  return ir::Value(ir::GenericFn(
      [](core::Arguments<type::Typed<ir::Value>> const &args) mutable
      -> ir::NativeFn { NOT_YET(); }));
}

WorkItem::Result Compiler::CompleteStruct(
    ast::ParameterizedStructLiteral const *node) {
  LOG("struct", "Completing struct-literal emission: %p must-complete = %s",
      node, state_.must_complete ? "true" : "false");

  type::Struct *s = context().get_struct(node);
  if (s->completeness() == type::Completeness::Complete) {
    LOG("struct", "Already complete, exiting: %p", node);
    return WorkItem::Result::Success;
  }

  ASSIGN_OR(return WorkItem::Result::Failure,  //
                   auto fn, StructCompletionFn(*this, s, node->fields()));
  // TODO: What if execution fails.
  interpreter::Execute<instruction_set_t>(std::move(fn));
  s->complete();
  LOG("struct", "Completed %s which is a struct %s with %u field(s).",
      node->DebugString(), *s, s->fields().size());
  return WorkItem::Result::Success;
}

}  // namespace compiler
