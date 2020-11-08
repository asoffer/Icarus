#include <vector>

#include "compiler/compiler.h"
#include "core/arguments.h"
#include "core/params.h"
#include "ir/value/value.h"
#include "type/qual_type.h"
#include "type/type.h"
#include "type/typed_value.h"

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

ir::Value PrepareOneArg(Compiler &c, type::Typed<ir::Value> const &arg,
                        type::Type param_type) {
  auto &bldr = c.builder();
  // TODO: other implicit conversions?
  auto t = arg.type();
  if (t.get()->is_big()) {
    auto r = bldr.TmpAlloca(t);
    c.EmitMoveInit(arg, type::Typed<ir::Reg>(r, t));
    return ir::Value(r);
  } else {
    return arg.get();
  }
}

}  // namespace

std::vector<ir::Value> Compiler::PrepareCallArguments(
    type::Type state_ptr_type, core::Params<type::QualType> const &params,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  std::vector<ir::Value> arg_values;
  arg_values.reserve(params.size());

  size_t i = 0;
  size_t j = 0;
  if (state_ptr_type) {
    arg_values.push_back(PrepareOneArg(*this, args[i++], state_ptr_type));
  }
  while (i < args.pos().size()) {
    arg_values.push_back(
        PrepareOneArg(*this, args[i++], params[j++].value.type()));
  }

  for (; i < params.size(); ++i) {
    arg_values.push_back(
        PrepareOneArg(*this, args[params[i].name], params[i].value.type()));
  }

  return arg_values;
}

std::optional<ir::CompiledFn> StructCompletionFn(
    Compiler &c, type::Struct *s, absl::Span<ast::Declaration const> fields) {
  ASSERT(s->completeness() != type::Completeness::Complete);
  bool field_error = false;
  for (auto const &field : fields) {
    auto const *qt = c.context().qual_type(&field);
    if (not qt or
        qt->type().get()->completeness() != type::Completeness::Complete) {
      c.diag().Consume(IncompleteField{.range = field.range()});
      field_error = true;
    }
  }

  if (field_error) { return std::nullopt; }

  ir::CompiledFn fn(type::Func({}, {}),
                    core::Params<type::Typed<ast::Declaration const *>>{});
  ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
    // TODO this is essentially a copy of the body of FunctionLiteral::EmitValue
    // Factor these out together.
    c.builder().CurrentBlock() = fn.entry();

    std::vector<type::StructInstruction::Field> ir_fields;

    bool has_field_needing_destruction = false;
    std::optional<ir::Fn> user_dtor;
    std::vector<ir::Fn> assignments;
    for (auto const &field : fields) {
      // TODO: Decide whether to support all hashtags. For now just covering
      // export.
      if (field.id() == "destroy") {
        // TODO: handle potential errors here.
        user_dtor = c.EmitValue(field.init_val()).get<ir::Fn>();
      } else if (field.id() == "assign") {
        // TODO handle potential errors here.
        assignments.push_back(c.EmitValue(field.init_val()).get<ir::Fn>());
      } else {
        type::Type field_type;
        if (auto *init_val = field.init_val()) {
          // TODO init_val type may not be the same.
          field_type = c.context().qual_type(init_val)->type();
          ir_fields.emplace_back(field.id(), field_type, c.EmitValue(init_val));
          ir_fields.back().set_export(
              field.hashtags.contains(ir::Hashtag::Export));
        } else {
          field_type = c.EmitValue(field.type_expr()).get<type::Type>();
          ir_fields.emplace_back(field.id(), field_type);
          ir_fields.back().set_export(
              field.hashtags.contains(ir::Hashtag::Export));
        }
        has_field_needing_destruction =
            has_field_needing_destruction or field_type.get()->HasDestructor();
      }
    }

    std::optional<ir::Fn> dtor;
    if (has_field_needing_destruction) {
      auto [full_dtor, inserted] = c.context().InsertDestroy(s);
      if (inserted) {
        ICARUS_SCOPE(ir::SetCurrent(full_dtor, c.builder())) {
          c.builder().CurrentBlock() = c.builder().CurrentGroup()->entry();
          auto var                   = ir::Reg::Arg(0);
          if (user_dtor) {
            // TODO: Should probably force-inline this.
            c.builder().Call(*user_dtor, full_dtor.type(), {ir::Value(var)},
                             ir::OutParams());
          }
          for (int i = fields.size() - 1; i >= 0; --i) {
            // TODO: Avoid emitting IR if the type doesn't need to be
            // destroyed.
            c.EmitDestroy(
                type::Typed<ir::Reg>(c.builder().FieldRef(var, s, i)));
          }

          c.builder().ReturnJump();
          full_dtor->WriteByteCode<interpretter::instruction_set_t>();

          dtor = full_dtor;
        }
      }
    } else {
      if (user_dtor) { dtor = *user_dtor; }
    }

    // If no assignments are specified, add a compiler-generated default.
    if (assignments.empty()) {
      auto [fn, inserted] = c.context().root().InsertMoveAssign(s, s);
      if (inserted) {
        ICARUS_SCOPE(ir::SetCurrent(fn, c.builder())) {
          c.builder().CurrentBlock() = fn->entry();
          auto var                   = ir::Reg::Arg(0);
          auto val                   = ir::Reg::Arg(1);

          for (size_t i = 0; i < s->fields().size(); ++i) {
            c.EmitMoveAssign(c.builder().FieldRef(var, s, i),
                             c.builder().FieldValue(val, s, i));
          }

          c.builder().ReturnJump();
        }
        fn->WriteByteCode<interpretter::instruction_set_t>();
      }
      assignments.push_back(fn);
    }

    c.current_block()->Append(
        type::StructInstruction{.struct_     = s,
                                .fields      = std::move(ir_fields),
                                .assignments = std::move(assignments),
                                .dtor        = dtor});
    c.builder().ReturnJump();
  }

  fn.WriteByteCode<interpretter::instruction_set_t>();

  return fn;
}

}  // namespace compiler
