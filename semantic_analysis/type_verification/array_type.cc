#include "ast/ast.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct NonIntegralArrayLength {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-integral-array-length";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Array length indexed by non-integral type"),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

struct ArrayDataTypeNotAType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "array-data-type-not-a-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Array type has underlying data type specified as a value which "
            "is not a type."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

struct NonTypeArrayTypeMatch {
  static constexpr std::string_view kCategory = "pattern-error";
  static constexpr std::string_view kName     = "non-type-array-type-match";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to match an array type against a value of type `%s`.",
            type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
  std::string type;
};

}  // namespace

// Verifies that the array type has constant integer lengths and that the data
// type expression is a type.
VerificationTask TypeVerifier::VerifyType(ast::ArrayType const *node) {
  std::vector<QualifiedType> length_results;
  length_results.reserve(node->lengths().size());
  bool error    = false;
  bool constant = true;
  for (auto const *length : node->lengths()) {
    std::span result = co_await VerifyTypeOf(length);
    if (result.size() != 1) { NOT_YET(); }
    if (not(result[0].qualifiers() >= Qualifiers::Constant())) {
      constant = false;
    }
    length_results.push_back(result[0]);

    if (not IsIntegral(result[0].type())) {
      ConsumeDiagnostic(NonIntegralArrayLength{.view = node->range()});
      error = true;
    }
  }

  std::span data_qts = co_await VerifyTypeOf(&node->data_type());

  if (data_qts.size() != 1) { NOT_YET(); }
  if (data_qts[0].type() != Type) {
    ConsumeDiagnostic(ArrayDataTypeNotAType{
        .view = node->data_type().range(),
    });
    error = true;
  } else if (not(data_qts[0].qualifiers() >= Qualifiers::Constant())) {
    constant = false;
  }

  co_return TypeOf(
      node, QualifiedType(
                Type, ((error ? Qualifiers::Error() : Qualifiers()) |
                       (constant ? Qualifiers::Constant() : Qualifiers()))));
}

}  // namespace semantic_analysis
