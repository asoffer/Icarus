#include "ast/ast.h"
#include "semantic_analysis/type_verification/verify.h"
#include "semantic_analysis/type_verification/casting.h"

namespace semantic_analysis {
namespace {

struct MultipleValuesAssignedToEnumerator {
  static constexpr std::string_view kCategory = "value-error";
  static constexpr std::string_view kName =
      "multiple-values-assigned-to-enumerator";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Enumerators must be assigned exactly one value, but "
                         "you provided an initializer with %u values.",
                         count),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  size_t count;
  std::string_view view;
};

struct NonConstantEnumerator {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-enumerator";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Values for enumerators must be declared as constants."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};
struct NonIntegralEnumerator {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-integral-enumerator";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Values for enumerators must be integers, but we "
                         "found an enumerator of type `%s`.",
                         type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
  std::string type;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(ast::EnumLiteral const *node) {
  co_yield TypeOf(node, Constant(Type));

  for (auto const &[name, value] : node->specified_values()) {
    auto qts = co_await VerifyTypeOf(value.get());
    if (qts.size() != 1) {
      ConsumeDiagnostic(MultipleValuesAssignedToEnumerator{
          .count = qts.size(), .view = value->range()});
      continue;
    }
    if (not(qts[0].qualifiers() >= Qualifiers::Constant())) {
      ConsumeDiagnostic(NonConstantEnumerator{.view = value->range()});
    }
    if (not IsIntegral(qts[0].type())) {
      ConsumeDiagnostic(NonIntegralEnumerator{
          .view = value->range(),
          .type = TypeForDiagnostic(*value),
      });
    }
  }

  co_return Completed(node);
}

}  // namespace semantic_analysis

