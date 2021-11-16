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
  LOG("StructLiteral", "Starting struct-literal emission: %p", node);

  auto [t, inserted] = context().EmplaceType<type::Struct>(
      node, resources().module,
      type::Struct::Options{
          .is_copyable = not node->hashtags.contains(ir::Hashtag::Uncopyable),
          .is_movable  = not node->hashtags.contains(ir::Hashtag::Immovable),
      });

  if (inserted) {
    // Strictly speaking this conditional is not needed. Enqueuing the same work
    // item twice will be deduplicated.
    Enqueue({.kind    = WorkItem::Kind::CompleteStructData,
             .node    = node,
             .context = &context()},
            {WorkItem{.kind    = WorkItem::Kind::VerifyStructBody,
                      .node    = node,
                      .context = &context()}});
  }
  out.append(t);
}

bool Compiler::CompleteStructData(ast::StructLiteral const *node) {
  LOG("StructLiteral", "Completing struct data: %p", node);

  // TODO: One of here or above should be responsible for EmplaceType, but not
  // both. If we could enqueue code emission, that would make sense.
  auto [t, inserted] = context().EmplaceType<type::Struct>(
      node, resources().module,
      type::Struct::Options{
          .is_copyable = not node->hashtags.contains(ir::Hashtag::Uncopyable),
          .is_movable  = not node->hashtags.contains(ir::Hashtag::Immovable),
      });
  // TODO: Find a way around these const casts.
  type::Struct *s = &const_cast<type::Struct &>(t.as<type::Struct>());
  if (s->completeness() != type::Completeness::Incomplete) { return true; }

  ASSIGN_OR(return false,  //
                   auto fn, StructDataCompletionFn(*this, s, node->fields()));

  // TODO: What if execution fails.
  InterpretAtCompileTime(fn);

  absl::flat_hash_set<WorkItem> prerequisites;

  for (auto const &field : node->fields()) {
    if (field.flags() & ast::Declaration::f_IsConst) {
      prerequisites.insert({.kind    = WorkItem::Kind::VerifyType,
                            .node    = &field,
                            .context = &context()});
    }
  }

  Enqueue({.kind    = WorkItem::Kind::CompleteStruct,
           .node    = node,
           .context = &context()}, std::move(prerequisites));

  LOG("StructLiteral",
      "Completed data for %s which is a struct %s with %u field(s).",
      node->DebugString(), *s, s->fields().size());
  return true;
}

bool Compiler::CompleteStruct(ast::StructLiteral const *node) {
  LOG("StructLiteral", "Completing struct-literal emission: %p", node);

  // TODO: Find a way around these const casts.
  type::Struct *s =
      &const_cast<type::Struct &>(context().LoadType(node).as<type::Struct>());
  if (s->completeness() == type::Completeness::Complete) {
    LOG("StructLiteral", "Already complete, exiting: %p", node);
    return true;
  }

  ASSIGN_OR(return false,  //
                   auto fn, StructCompletionFn(*this, s, node->fields()));
  // TODO: What if execution fails.
  InterpretAtCompileTime(fn);

  LOG("StructLiteral", "Completed %s which is a struct %s with %u field(s).",
      node->DebugString(), *s, s->fields().size());
  return true;
}

}  // namespace compiler
