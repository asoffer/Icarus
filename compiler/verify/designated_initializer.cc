#include "ast/ast.h"
#include "base/defer.h"
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

  std::vector<std::vector<type::QualType>> initializer_qts;
  initializer_qts.reserve(node->assignments().size());
  for (auto const *assignment : node->assignments()) {
    auto &back = initializer_qts.emplace_back();
    for (auto const *expr : assignment->rhs()) {
      back.push_back(VerifyType(expr));
    }
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
  for (auto const *assignment : node->assignments()) {
    auto const &initializer_qt_vec = *initializer_iter;
    ++initializer_iter;

    size_t qt_index  = 0;
    size_t qt_offset = 0;
    DEBUG_LOG()(assignment->lhs().size());
    for (auto const *field : assignment->lhs()) {
      DEBUG_LOG()(field->DebugString());
      std::string_view field_name   = field->as<ast::Identifier>().token();
      type::QualType initializer_qt = initializer_qt_vec[qt_index];
      if (not initializer_qt.ok()) {
        // If there was an error we still want to verify all other initializers
        // and we still want to claim this expression has the same type, but
        // we'll just give up on it being a constant.
        quals           = type::Quals::Unqualified();
        recovered_error = true;
        DEBUG_LOG()("HUH????");
        goto next_assignment;
      }

      type::Type const *t = [&] {
        if (initializer_qt.expansion_size() == 1) {
          ++qt_offset;
          return initializer_qt.type();
        } else {
          return initializer_qt.expanded()[qt_offset++];
        }
      }();

      base::defer d = [&] {
        if (qt_offset == initializer_qt.expansion_size()) {
          qt_offset = 0;
          ++qt_index;
        }
      };

      if (auto *struct_field = struct_type->field(field_name)) {
        if (not type::CanCast(t, struct_field->type)) {
          diag().Consume(InvalidInitializerType{
              .expected = struct_field->type,
              .actual   = t,
              .range    = field->range(),
          });
          recovered_error = true;
          quals           = type::Quals::Unqualified();
        } else {
          quals &= initializer_qt.quals();
        }
      } else {
        diag().Consume(MissingStructField{
            .member_name = std::string(field_name),
            .struct_type = struct_type,
            .range       = field->range(),
        });
        recovered_error = true;
        quals           = type::Quals::Unqualified();
      }
    }

  next_assignment:;
  }

  type::QualType qt(struct_type, quals);
  if (recovered_error) { qt.MarkError(); }
  return data().set_qual_type(node, qt);
}

}  // namespace compiler
