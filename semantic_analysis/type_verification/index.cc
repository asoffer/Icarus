#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct UnexpandedExpression {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "unexpanded-expression";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Expected an expression evaluating to exactly one value, but the "
            "provided expression expands to %u value(s).",
            num_arguments),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  size_t num_arguments;
  std::string_view view;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(ast::Index const *node) {
  std::span lhs_qts = co_await VerifyTypeOf(node->lhs());
  std::span rhs_qts = co_await VerifyTypeOf(node->rhs());

  QualifiedType qt;

  if (lhs_qts.size() != 1) {
    ConsumeDiagnostic(UnexpandedExpression{
        .num_arguments = lhs_qts.size(),
        .view          = node->lhs()->range(),
    });

    qt = Error();
  }
  if (rhs_qts.size() != 1) {
    ConsumeDiagnostic(UnexpandedExpression{
        .num_arguments = rhs_qts.size(),
        .view          = node->rhs()->range(),
    });

    qt = Error();
  }

  if (qt == Error()) { co_return TypeOf(node, qt); }

  if (auto slice_type = lhs_qts[0].type().get_if<SliceType>(type_system())) {
    qt = QualifiedType(slice_type->pointee(), Qualifiers::Reference());
  } else {
    NOT_YET();
  }

  co_return TypeOf(node, qt);
}

}  // namespace semantic_analysis
