#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "base/defer.h"
#include "compiler/compiler.h"
#include "compiler/verify/internal/qual_type_iterator.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/struct.h"

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

  type::Type type;
  frontend::SourceRange range;
};

struct NonStructDesignatedInitializer {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "non-struct-designated-initializer-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Designated initializers can only be used with structs, but you "
            "provided a `%s`",
            type.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type type;
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
            expected.to_string(), actual.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type expected;
  type::Type actual;
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
  ASSIGN_OR(return type::QualType::Error(), type::Type t,
                   EvaluateOrDiagnoseAs<type::Type>(node->type()));

  auto *struct_type = t.if_as<type::Struct>();
  if (not struct_type) {
    diag().Consume(NonStructDesignatedInitializer{
        .type  = t,
        .range = node->type()->range(),
    });
    return type::QualType::Error();
  }

  bool recovered_error  = false;
  type::Quals quals     = type::Quals::Const();
  auto initializer_iter = initializer_qts.begin();
  for (auto const *assignment : node->assignments()) {
    auto const &initializer_qt_vec = *initializer_iter;
    base::defer d                  = [&] { ++initializer_iter; };

    // Check all struct fields first to ensure we generate errors for them even
    // if there are errors in the initializers.

    absl::flat_hash_map<std::string_view, type::Struct::Field const *>
        name_to_field;
    for (auto const *field : assignment->lhs()) {
      std::string_view field_name = field->as<ast::Identifier>().name();
      if (auto *struct_field = struct_type->field(field_name)) {
        name_to_field.emplace(field_name, struct_field);
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

    internal::QualTypeIterator qt_iter(initializer_iter->begin());
    internal::QualTypeIterator const qt_end(initializer_iter->end());

    for (auto const *field : assignment->lhs()) {
      std::string_view field_name   = field->as<ast::Identifier>().name();
      type::QualType initializer_qt = *qt_iter;
      ++qt_iter;

      if (not initializer_qt.ok()) {
        // If there was an error we still want to verify all other initializers
        // and we still want to claim this expression has the same type, but
        // we'll just give up on it being a constant.
        quals           = type::Quals::Unqualified();
        recovered_error = true;
        goto next_assignment;
      }

      auto iter = name_to_field.find(field_name);
      if (iter == name_to_field.end()) { continue; }

      type::Type lhs_type = initializer_qt.type();
      type::Type rhs_type = iter->second->type;
      if (not type::CanCast(lhs_type, rhs_type)) {
        diag().Consume(InvalidInitializerType{
            .expected = rhs_type,
            .actual   = initializer_qt.type(),
            .range    = field->range(),
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
