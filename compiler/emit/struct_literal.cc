#include <utility>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/instructions.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::StructLiteral const *node) {
  LOG("StructLiteral", "Starting struct-literal emission: %p%s", node,
      state_.must_complete ? " (must complete)" : " (need not complete)");

  if (type::Struct *s = context().get_struct(node)) {
    LOG("StructLiteral", "Early return with possibly incomplete type %p", s);
    return ir::Value(type::Type(s));
  }

  type::Struct *s = type::Allocate<type::Struct>(
      &context().module(),
      type::Struct::Options{
          .is_copyable = not node->hashtags.contains(ir::Hashtag::Uncopyable),
          .is_movable  = not node->hashtags.contains(ir::Hashtag::Immovable),
      });

  LOG("struct", "Allocating a new struct %p for %p on context %p", s, node,
      &context());
  context().set_struct(node, s);

  // Note: VerifyBody may end up triggering EmitValue calls for member types
  // that depend on this incomplete type. For this reason it is important that
  // we have already allocated the struct so we do not get a double-allocation.
  //
  // The process, as you can see above is to
  // 1. Check if it has already been allocated. Return if it has been.
  // 2. Allocate ourselves.
  // 3. Start body verification.
  // 4. Schedule completion.
  //
  // Notably, steps 1 and 2 must not be separated. Moreover, because body
  // verification could end up calling this function again, we must "set up
  // guards" (i.e., steps 1 and 2) before step 3 runs.

  if (state_.must_complete) {
    LOG("compile-work-queue", "Request work complete struct: %p", node);
    state_.work_queue.Enqueue({
        .kind      = WorkItem::Kind::CompleteStructMembers,
        .node      = node,
        .resources = resources_,
    });
  }
  return ir::Value(type::Type(s));
}

WorkItem::Result Compiler::CompleteStruct(ast::StructLiteral const *node) {
  LOG("StructLiteral", "Completing struct-literal emission: %p must-complete = %s",
      node, state_.must_complete ? "true" : "false");

  // TODO: Check the result of body verification.
  if (state_.must_complete and context().ShouldVerifyBody(node)) {
    VerifyBody(node);
  }

  type::Struct *s = context().get_struct(node);
  if (s->completeness() == type::Completeness::Complete) {
    LOG("StructLiteral", "Already complete, exiting: %p", node);
    return WorkItem::Result::Success;
  }

  ASSIGN_OR(return WorkItem::Result::Failure,  //
                   auto fn, StructCompletionFn(*this, s, node->fields()));
  // TODO: What if execution fails.
  InterpretAtCompileTime(ir::NativeFn(&fn));
  s->complete();
  LOG("StructLiteral", "Completed %s which is a struct %s with %u field(s).",
      node->DebugString(), *s, s->fields().size());
  return WorkItem::Result::Success;
}

}  // namespace compiler
