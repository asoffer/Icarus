#include "ast/ast.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/casting.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct TypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "assignment-type-mismatch";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Cannot assign a value of type `%s` to a reference of type `%s`%s:",
            rhs_type, lhs_type, castable ? " without an explicit cast" : ""),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string lhs_type;
  std::string rhs_type;
  std::string_view view;
  bool castable;
};

struct AssigningToNonReference {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "assigning-to-non-reference";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Assigning to a non-reference expression:"),
        diagnostic::SourceQuote().Highlighted(lhs,
                                              diagnostic::Style::ErrorText()));
  }

  std::string_view lhs;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Assignment const *node) {
  std::vector<QualifiedType> rhs_qts;
  std::vector<core::Type> lhs_types;
  for (auto const *l : node->lhs()) {
    std::span qts = co_await VerifyTypeOf(l);
    if (qts.size() != 1) { NOT_YET(); }
    if (not(qts[0].qualifiers() >= Qualifiers::Reference())) {
      tv.ConsumeDiagnostic(AssigningToNonReference{.lhs = l->range()});
    }
    lhs_types.push_back(qts[0].type());
  }

  for (auto const *r : node->rhs()) {
    std::span qts = co_await VerifyTypeOf(r);
    if (qts.size() != 1) { NOT_YET(); }
    rhs_qts.push_back(qts[0]);
  }

  if (lhs_types.size() != rhs_qts.size()) { NOT_YET(); }

  for (size_t i = 0; i < lhs_types.size(); ++i) {
    auto kind = CanCast(rhs_qts[i], lhs_types[i], tv.type_system());
    switch (kind) {
      case CastKind::None:
      case CastKind::Explicit:
        tv.ConsumeDiagnostic(TypeMismatch{
            .lhs_type = tv.TypeForDiagnostic(*node->lhs()[i]),
            .rhs_type = tv.TypeForDiagnostic(*node->rhs()[i]),
            // TODO: set the range to point more directly to the things we
            // care about.
            .view     = node->range(),
            .castable = (kind == CastKind::Explicit),
        });
        break;
      case CastKind::Implicit:
      case CastKind::InPlace: continue;
    }
  }

  co_return tv.Completed(node);
}

}  // namespace semantic_analysis
