#include "compiler/struct.h"

#include <utility>
#include <vector>

#include "compiler/instructions.h"

namespace compiler {

void EmitStructDataCompletion(CompilationDataReference c,
                              ir::RegOr<type::Type> s,
                              absl::Span<ast::Declaration const> field_decls) {
  std::vector<type::StructDataInstruction::Field> fields;

  for (auto const &field_decl : field_decls) {
    if (field_decl.flags() & ast::Declaration::f_IsConst) { continue; }
    // TODO: Access to init_val is not correct here because that may
    // initialize multiple values.
    for (auto const &id : field_decl.ids()) {
      // TODO: Decide whether to support all hashtags. For now just covering
      // export.
      if (auto const *init_val = id.declaration().init_val()) {
        type::Type field_type;
        if (auto const *type_expr = id.declaration().type_expr()) {
          std::optional t = c.EvaluateOrDiagnoseAs<type::Type>(type_expr);
          ASSERT(t.has_value() == true);
          field_type = *t;
        } else {
          field_type = c.context().qual_types(init_val)[0].type();
        }

        // TODO: If the initializer needs a cast.
        if (field_type != c.context().qual_types(init_val)[0].type()) {
          NOT_YET();
        }
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
}

}  // namespace compiler
