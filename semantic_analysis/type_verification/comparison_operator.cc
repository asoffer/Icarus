#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct ComparingIncomparables {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "comparing-incomparables";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Values of type `%s` and `%s` are being compared but no such "
            "comparison is allowed:",
            lhs, rhs),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string lhs;
  std::string rhs;
  std::string_view view;
};

bool CanCompare(core::Type lhs, core::Type rhs, frontend::Operator op,
                TypeSystem &ts) {
  // TODO: This implementation is incomplete and incorrect.
  if (IsIntegral(lhs) and IsIntegral(rhs)) {
    return lhs == rhs or lhs == Integer or rhs == Integer;
  }
  NOT_YET();
}

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::ComparisonOperator const *node) {
  std::vector<core::Type> operand_types;
  Qualifiers qualifiers = Qualifiers::Constant();
  operand_types.reserve(node->exprs().size());
  bool all_length_one = true;
  for (auto const *expr : node->exprs()) {
    std::span expr_qts = co_await VerifyTypeOf(expr);
    if (expr_qts.size() == 1) {
      operand_types.push_back(expr_qts.front().type());
      qualifiers &= expr_qts.front().qualifiers();
    } else {
      all_length_one = false;
    }
  }
  if (not all_length_one) { co_return tv.TypeOf(node, Error(Bool)); }

  QualifiedType qt(Bool, qualifiers);
  for (size_t i = 0; i < node->ops().size(); ++i) {
    auto const *lhs = node->exprs()[i];
    auto const *rhs = node->exprs()[i + 1];
    auto lhs_type  = operand_types[i];
    auto rhs_type  = operand_types[i + 1];

    if (not CanCompare(lhs_type, rhs_type, node->ops()[i], tv.type_system())) {
      tv.ConsumeDiagnostic(ComparingIncomparables{
          .lhs  = tv.TypeForDiagnostic(*lhs),
          .rhs  = tv.TypeForDiagnostic(*rhs),
          .view = node->binary_range(i),
      });
      qt = Error(Bool);
    }
  }
  co_return tv.TypeOf(node, qt);
}

}  // namespace semantic_analysis
