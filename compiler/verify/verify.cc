#include "compiler/verify/verify.h"

#include "compiler/instructions.h"
#include "compiler/struct.h"

namespace compiler {

absl::Span<type::QualType const> VerifyType(CompilationDataReference data,
                                            ast::Node const *node) {
  return TypeVerifier(data)(node);
}

bool VerifyBody(CompilationDataReference data, ast::Node const *node) {
  return BodyVerifier(data)(node);
}

bool VerifyPatternType(CompilationDataReference data, ast::Node const *node,
                       type::Type t) {
  return PatternTypeVerifier(data)(node, t);
}

std::optional<ir::Subroutine> StructDataCompletionFn(
    CompilationDataReference c, type::Struct *s,
    absl::Span<ast::Declaration const> field_decls) {
  ASSERT(s->completeness() == type::Completeness::Incomplete);
  ir::Subroutine fn(type::Func({}, {}));
  c.push_current(&fn);
  absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
  c.current_block()     = fn.entry();

  bool field_error = false;
  for (auto const &field_decl : field_decls) {
    if (field_decl.flags() & ast::Declaration::f_IsConst) { continue; }
    for (auto const &id : field_decl.ids()) {
      type::QualType qt = c.context().qual_types(&id)[0];
      if (not qt or
          qt.type().get()->completeness() == type::Completeness::Incomplete) {
        c.diag().Consume(IncompleteField{.range = id.range()});
        field_error = true;
      }
    }
  }

  if (field_error) { return std::nullopt; }

  EmitStructDataCompletion(c, type::Type(s), field_decls);
  c.current_block()->set_jump(ir::JumpCmd::Return());
  return fn;
}

bool CompleteStructData(CompilationDataReference data,
                        ast::StructLiteral const *node) {
  LOG("StructLiteral", "Completing struct data: %p", node);

  // TODO: One of here or above should be responsible for EmplaceType, but not
  // both. If we could enqueue code emission, that would make sense.
  //
  // TODO: Get rid of reinterpret cast needed because of incomplete type.
  auto [t, inserted] = data.context().EmplaceType<type::Struct>(
      node,
      reinterpret_cast<module::Module const *>(data.resources().module),
      type::Struct::Options{
          .is_copyable = not node->hashtags.contains(ir::Hashtag::Uncopyable),
          .is_movable  = not node->hashtags.contains(ir::Hashtag::Immovable),
      });
  // TODO: Find a way around these const casts.
  type::Struct *s = &const_cast<type::Struct &>(t.as<type::Struct>());
  if (s->completeness() != type::Completeness::Incomplete) { return true; }

  ASSIGN_OR(return false,  //
                   auto fn, StructDataCompletionFn(data, s, node->fields()));

  // TODO: What if execution fails.
  InterpretAtCompileTime(data.shared_context(), fn);

  absl::flat_hash_set<WorkItem> prerequisites;

  for (auto const &field : node->fields()) {
    if (field.flags() & ast::Declaration::f_IsConst) {
      prerequisites.insert({.kind    = WorkItem::Kind::VerifyType,
                            .node    = &field,
                            .context = &data.context()});
    }
  }

  data.Enqueue({.kind    = WorkItem::Kind::CompleteStruct,
                .node    = node,
                .context = &data.context()},
               std::move(prerequisites));

  LOG("StructLiteral",
      "Completed data for %s which is a struct %s with %u field(s).",
      node->DebugString(), *s, s->fields().size());
  return true;
}

bool CompleteStructData(CompilationDataReference data,
                        ast::ParameterizedStructLiteral const *node) {
  LOG("ParameterizedStructLiteral", "Completing struct data: %p", node);

  // TODO: One of here or above should be responsible for EmplaceType, but not
  // both. If we could enqueue code emission, that would make sense.
  //
  // TODO: Get rid of reinterpret cast needed because of incomplete type.
  auto [t, inserted] = data.context().EmplaceType<type::Struct>(
      node,
      reinterpret_cast<module::Module const *>(data.resources().module),
      type::Struct::Options{
          .is_copyable = not node->hashtags.contains(ir::Hashtag::Uncopyable),
          .is_movable  = not node->hashtags.contains(ir::Hashtag::Immovable),
      });
  // TODO: Find a way around these const casts.
  type::Struct *s = &const_cast<type::Struct &>(t.as<type::Struct>());
  if (s->completeness() != type::Completeness::Incomplete) { return true; }

  ASSIGN_OR(return false,  //
                   auto fn, StructDataCompletionFn(data, s, node->fields()));

  // TODO: What if execution fails.
  InterpretAtCompileTime(data.shared_context(), fn);

  absl::flat_hash_set<WorkItem> prerequisites;

  for (auto const &field : node->fields()) {
    if (field.flags() & ast::Declaration::f_IsConst) {
      prerequisites.insert({.kind    = WorkItem::Kind::VerifyType,
                            .node    = &field,
                            .context = &data.context()});
    }
  }

  data.Enqueue({.kind    = WorkItem::Kind::CompleteStruct,
                .node    = node,
                .context = &data.context()},
               std::move(prerequisites));

  LOG("StructLiteral",
      "Completed data for %s which is a struct %s with %u field(s).",
      node->DebugString(), *s, s->fields().size());
  return true;
}

}  // namespace compiler
