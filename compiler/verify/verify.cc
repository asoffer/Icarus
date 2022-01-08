#include "compiler/verify/verify.h"
#include "compiler/instructions.h"

namespace compiler {
namespace {

struct IncompleteField {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "incomplete-field";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Struct field has incomplete type."),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
};

}  // namespace

bool VerifyBody(CompilationDataReference data, ast::Node const *node) {
  return BodyVerifier(data)(node);
}

std::optional<ir::CompiledFn> StructDataCompletionFn(
    CompilationDataReference c, type::Struct *s,
    absl::Span<ast::Declaration const> field_decls) {
  ASSERT(s->completeness() == type::Completeness::Incomplete);
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

  ir::CompiledFn fn(type::Func({}, {}));
  ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
    c.builder().CurrentBlock() = fn.entry();

    std::vector<type::StructDataInstruction::Field> fields;

    for (auto const &field_decl : field_decls) {
      if (field_decl.flags() & ast::Declaration::f_IsConst) { continue; }
      // TODO: Access to init_val is not correct here because that may
      // initialize multiple values.
      for (auto const &id : field_decl.ids()) {
        // TODO: Decide whether to support all hashtags. For now just covering
        // export.
        if (auto const *init_val = id.declaration().init_val()) {
          // TODO init_val type may not be the same.
          type::Type field_type = c.context().qual_types(init_val)[0].type();

          ASSIGN_OR(
              NOT_YET(),  //
              auto result,
              c.EvaluateToBufferOrDiagnose(type::Typed(init_val, field_type)));

          fields.emplace_back(id.name(), field_type, std::move(result))
              .set_export(
                  id.declaration().hashtags.contains(ir::Hashtag::Export));
        } else {
          // TODO: Failed evaluation
          // TODO: Type expression actually refers potentially to multiple
          // declaration ids.
          type::Type field_type =
              c.EvaluateOrDiagnoseAs<type::Type>(id.declaration().type_expr())
                  .value();
          fields.emplace_back(id.name(), field_type)
              .set_export(
                  id.declaration().hashtags.contains(ir::Hashtag::Export));
        }
      }
    }

    c.current_block()->Append(
        type::StructDataInstruction{.struct_ = s, .fields = std::move(fields)});
    c.builder().ReturnJump();
  }

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
      reinterpret_cast<module::BasicModule const *>(data.resources().module),
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
  InterpretAtCompileTime(fn);

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
