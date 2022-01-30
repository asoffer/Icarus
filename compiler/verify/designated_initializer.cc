#include "absl/cleanup/cleanup.h"
#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/module.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/verify.h"
#include "type/cast.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/struct.h"

namespace compiler {
namespace {

struct NonConstantDesignatedInitializerType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "non-constant-designated-initializer-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Designated initializer type must be a constant."),
        diagnostic::SourceQuote()
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
};

struct NonTypeDesignatedInitializerType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "non-type-designated-initializer-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Designated initializer type must be a type, but you "
                         "provided an expression which is a `%s`.",
                         type),
        diagnostic::SourceQuote()
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  std::string type;
  frontend::SourceView view;
};

struct NonStructDesignatedInitializer {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "non-struct-designated-initializer-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Designated initializers can only be used with structs, but you "
            "provided a `%s`",
            type),
        diagnostic::SourceQuote()
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  std::string type;
  frontend::SourceView view;
};

struct InvalidInitializerType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-initializer-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Designated initializer field encountered an unexpected type:\n"
            "  Expected: A type convertible to `%s`\n"
            "  Actual:   `%s`",
            expected, actual),
        diagnostic::SourceQuote()
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  type::Type expected;
  type::Type actual;
  frontend::SourceView view;
};

struct NonExportedField {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-exported-field";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Field named `%s` in struct `%s` is not exported.",
                         member_name, type::Type(struct_type)),
        diagnostic::SourceQuote()
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  std::string member_name;
  type::Struct const *struct_type;
  frontend::SourceView view;
};

struct MissingStructField {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "missing-struct-field";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No field named `%s` in struct `%s`.", member_name,
                         struct_type),
        diagnostic::SourceQuote()
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  std::string member_name;
  std::string struct_type;
  frontend::SourceView view;
};

// Returns whether `qt` is a const type, emitting diagnostics for both constness
// and type-ness if it is not.
bool ValidateConstType(ast::DesignatedInitializer const &node,
                       type::QualType qt, Context const &context,
                       diagnostic::DiagnosticConsumer &diagnostic_consumer) {
  bool result = true;

  if (not qt.constant()) {
    diagnostic_consumer.Consume(NonConstantDesignatedInitializerType{
        .view = SourceViewFor(node.type()),
    });
    result = false;
  }

  if (qt.type() != type::Type_) {
    diagnostic_consumer.Consume(NonTypeDesignatedInitializerType{
        .type = TypeForDiagnostic(&node, context),
        .view = SourceViewFor(node.type()),
    });
    result = false;
  }
  return result;
}

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::DesignatedInitializer const *node) {
  auto type_qt = VerifyType(node->type())[0];
  if (not ValidateConstType(*node, type_qt, context(), diag())) {
    return context().set_qual_type(node, type::QualType::Error());
  }

  std::vector<std::vector<type::QualType>> initializer_qts(
      node->assignments().size());
  auto iter = initializer_qts.begin();
  for (auto const *assignment : node->assignments()) {
    auto &initializer_qt = *iter++;
    for (auto const *expr : assignment->rhs()) {
      auto span = VerifyType(expr);
      initializer_qt.insert(initializer_qt.end(), span.begin(), span.end());
    }
  }

  // Evaluate the type next so that the default ordering of error messages makes
  // sense (roughly top-to-bottom).

  ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),
                   type::Type t,
                   EvaluateOrDiagnoseAs<type::Type>(node->type()));

  auto *struct_type = t.if_as<type::Struct>();
  if (not struct_type) {
    diag().Consume(NonStructDesignatedInitializer{
        .type = t.to_string(),
        .view = SourceViewFor(node->type()),
    });
    return context().set_qual_type(node, type::QualType::Error());
  }

  if (auto *ast_struct = context().AstLiteral(struct_type)) {
    EnsureComplete({.kind    = WorkItem::Kind::CompleteStructData,
                    .node    = ast_struct,
                    .context = &context()});
  }
  ASSERT(struct_type->completeness() >= type::Completeness::DataComplete);

  bool recovered_error  = false;
  type::Quals quals     = type::Quals::Const();
  auto initializer_iter = initializer_qts.begin();
  for (auto const *assignment : node->assignments()) {
    auto const &initializer_qt_vec = *initializer_iter;
    absl::Cleanup c                = [&] { ++initializer_iter; };

    // Check all struct fields first to ensure we generate errors for them even
    // if there are errors in the initializers.

    absl::flat_hash_map<std::string_view, type::Struct::Field const *>
        name_to_field;
    for (auto const *field : assignment->lhs()) {
      std::string_view field_name = field->as<ast::Identifier>().name();
      if (auto *struct_field = struct_type->field(field_name)) {
        if (struct_type->defining_module() == resources().module or
            struct_field->hashtags.contains(ir::Hashtag::Export)) {
          name_to_field.emplace(field_name, struct_field);
        } else {
          diag().Consume(NonExportedField{
              .member_name = std::string(field_name),
              .struct_type = struct_type,
              .view        = SourceViewFor(field),
          });
          recovered_error = true;
          quals           = type::Quals::Unqualified();
        }
      } else {
        diag().Consume(MissingStructField{
            .member_name = std::string(field_name),
            .struct_type = TypeForDiagnostic(node, context()),
            .view        = SourceViewFor(field),
        });
        recovered_error = true;
        quals           = type::Quals::Unqualified();
      }
    }

    auto qt_iter = initializer_iter->begin();

    for (auto const *field : assignment->lhs()) {
      std::string_view field_name = field->as<ast::Identifier>().name();
      ASSERT(qt_iter != initializer_iter->end());
      type::QualType initializer_qt = *qt_iter;
      ++qt_iter;

      if (not initializer_qt.ok()) {
        // If there was an error we still want to verify all other initializers
        // and we still want to claim this expression has the same type, but
        // we'll just give up on it being a constant.
        recovered_error = true;
        quals           = type::Quals::Unqualified();
        goto next_assignment;
      }

      auto iter = name_to_field.find(field_name);
      if (iter == name_to_field.end()) { continue; }

      type::Type lhs_type = initializer_qt.type();
      type::Type rhs_type = iter->second->type;
      if (not type::CanCastImplicitly(lhs_type, rhs_type)) {
        diag().Consume(InvalidInitializerType{
            .expected = rhs_type,
            .actual   = lhs_type,
            .view     = SourceViewFor(field),
        });
        recovered_error = true;
        quals           = type::Quals::Unqualified();
      } else {
        quals &= initializer_qt.quals();
      }
    }

  next_assignment:;
  }

  type::QualType qt(struct_type, quals);
  if (recovered_error) { qt.MarkError(); }
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
