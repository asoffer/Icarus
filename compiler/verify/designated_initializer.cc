#include "ast/ast.h"
#include "compiler/compiler.h"
#include "diagnostic/errors.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct NonConstantDesignatedInitializerType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "non-constant-designated-initializer-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Designated initializer type must be a constant."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct NonTypeDesignatedInitializerType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "non-type-designated-initializer-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Designated initializer type must be a type, but you "
                         "provided an expression which is a `%s`."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type const *type;
  frontend::SourceRange range;
};

struct NonStructDesignatedInitializer {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "non-struct-designated-initializer-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Designated initializers can only be used with structs, but you "
             "provided a `%s`",
             type->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type const *type;
  frontend::SourceRange range;
};

struct InvalidInitializerType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-initializer-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Designated initializer field encountered an unexpected type:\n"
            "  Expected: A type convertible to `%s`\n"
            "  Actual:   `%s`",
            expected->to_string(), actual->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type const *expected;
  type::Type const *actual;
  frontend::SourceRange range;
};

struct MissingStructField {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "missing-struct-field";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No field named `%s` in struct `%s`.", member_name,
                         struct_type->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  std::string member_name;
  type::Struct const *struct_type;
  frontend::SourceRange range;
};

}  // namespace

type::QualType Compiler::VerifyType(ast::DesignatedInitializer const *node) {
  auto type_type = VerifyType(node->type());
  bool error     = false;
  // Check for constness and type-ness separately so we can emit different error
  // messages (potentially both). Then, if either failed, exit early.
  if (not type_type.constant()) {
    diag().Consume(NonConstantDesignatedInitializerType{
        .range = node->range(),
    });
    error = true;
  }

  if (type_type.type() != type::Type_) {
    diag().Consume(NonTypeDesignatedInitializerType{
        .type  = type_type.type(),
        .range = node->range(),
    });
    error = true;
  }
  if (error) { return type::QualType::Error(); }

  std::vector<type::QualType> initializer_qts;
  initializer_qts.reserve(node->assignments().size());
  for (auto &[field, expr] : node->assignments()) {
    initializer_qts.push_back(VerifyType(expr.get()));
  }

  // Evaluate the type next so that the default ordering of error messages makes
  // sense (roughly top-to-bottom).
  auto maybe_type = EvaluateAs<type::Type const *>(node->type());
  if (not maybe_type) {
    diag().Consume(diagnostic::EvaluationFailure{
        .failure = maybe_type.error(),
        .range   = node->type()->range(),
    });
    return type::QualType::Error();
  }

  auto *struct_type = ASSERT_NOT_NULL(*maybe_type)->if_as<type::Struct>();
  if (not struct_type) {
    diag().Consume(NonStructDesignatedInitializer{
        .type  = *maybe_type,
        .range = node->type()->range(),
    });
    return type::QualType::Error();
  }

  bool recovered_error  = false;
  type::Quals quals     = type::Quals::Const();
  auto initializer_iter = initializer_qts.begin();
  for (auto &[field, expr] : node->assignments()) {
    type::QualType initializer_qual_type = *initializer_iter;
    if (not initializer_qual_type) {
      // If there was an error we still want to verify all other initializers
      // and we still want to claim this expression has the same type, but
      // we'll just give up on it being a constant.
      quals           = type::Quals::Unqualified();
      recovered_error = true;
      continue;
    }

    if (auto *struct_field = struct_type->field(field)) {
      if (not type::CanCast(initializer_qual_type.type(), struct_field->type)) {
        diag().Consume(InvalidInitializerType{
            .expected = struct_field->type,
            .actual   = initializer_qual_type.type(),
            // TODO: this range is wrong, it should point to the field not the
            // expression initializing it. Skipping for now because we do not
            // save source ranges for this.
            .range = expr->range(),
        });
        recovered_error = true;
        quals           = type::Quals::Unqualified();
      } else {
        quals &= initializer_qual_type.quals();
      }
    } else {
      diag().Consume(MissingStructField{
          .member_name = field,
          .struct_type = struct_type,
          // TODO: this range is wrong, it should point to the field not the
          // expression initializing it. Skipping for now because we do not save
          // source ranges for this.
          .range = expr->range(),
      });
      recovered_error = true;
      quals           = type::Quals::Unqualified();
    }
  }

  type::QualType qt(struct_type, quals);
  if (recovered_error) { qt.MarkError(); }
  return data().set_qual_type(node, qt);
}

}  // namespace compiler
