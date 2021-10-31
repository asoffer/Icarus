#include <utility>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/instructions.h"
#include "compiler/module.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::StructLiteral const *node,
                            ir::PartialResultBuffer &out) {
  LOG("StructLiteral", "Starting struct-literal emission: %p%s", node,
      state_.must_complete ? " (must complete)" : " (need not complete)");

  if (type::Struct *s = context().get_struct(node)) {
    LOG("StructLiteral", "Early return with possibly incomplete type %p", s);
    out.append(type::Type(s));
    return;
  }

  type::Struct *s = type::Allocate<type::Struct>(
      &context().module(),
      type::Struct::Options{
          .is_copyable = not node->hashtags.contains(ir::Hashtag::Uncopyable),
          .is_movable  = not node->hashtags.contains(ir::Hashtag::Immovable),
      });

  LOG("StructLiteral", "Allocating a new struct %p for %p on context %p", s,
      node, &context());
  context().set_struct(node, s);

  // Note: VerifyBody may end up triggering EmitToBuffer calls for member types
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
    Enqueue({.kind    = WorkItem::Kind::CompleteStructMembers,
             .node    = node,
             .context = &context()},
            {WorkItem{.kind    = WorkItem::Kind::VerifyStructBody,
                      .node    = node,
                      .context = &context()}});
  }
  out.append(type::Type(s));
}

bool Compiler::CompleteStruct(ast::StructLiteral const *node) {
  LOG("StructLiteral", "Completing struct-literal emission: %p must-complete = %s",
      node, state_.must_complete ? "true" : "false");

  if (state_.must_complete and not context().BodyIsVerified(node)) {
    NOT_YET();
  }

  type::Struct *s = context().get_struct(node);
  if (s->completeness() == type::Completeness::Complete) {
    LOG("StructLiteral", "Already complete, exiting: %p", node);
    return true;
  }

  ASSIGN_OR(return false,  //
                   auto fn, StructCompletionFn(*this, s, node->fields()));
  // TODO: What if execution fails.
  InterpretAtCompileTime(fn);
  s->complete();
  LOG("StructLiteral", "Completed %s which is a struct %s with %u field(s).",
      node->DebugString(), *s, s->fields().size());
  return true;
}

}  // namespace compiler
