#include "ast/ast.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct NonTypeFunctionInput {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-type-function-input";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "The specified input type for a function must be a type."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }
  std::string_view view;
};

struct NonTypeFunctionOutput {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-type-function-output";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "The specified return type for a function must be a type."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }
  std::string_view view;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::FunctionType const *node) {
  Qualifiers qualifiers = Qualifiers::Constant();

  for (auto const *p : node->parameters()) {
    std::span parameter_qts = co_await VerifyTypeOf(p);
    if (parameter_qts.size() != 1) { NOT_YET("log an error"); }
    QualifiedType qt = parameter_qts[0];
    if (qt.qualifiers() >= Qualifiers::Error()) {
      qualifiers |= Qualifiers::Error();
      continue;
    }

    if (not p->is<ast::Declaration>()) {
      if (not(qt.qualifiers() >= Qualifiers::Constant())) {
        qualifiers &= ~Qualifiers::Constant();
      }
      if (qt.type() != Type) {
        tv.ConsumeDiagnostic(NonTypeFunctionInput{.view = p->range()});
        qualifiers |= Qualifiers::Error();
      }
    }
  }

  for (auto const *out : node->outputs()) {
    std::span out_qts = co_await VerifyTypeOf(out);
    if (out_qts.size() != 1) { NOT_YET("log an error"); }
    QualifiedType qt = out_qts[0];
    if (qt.qualifiers() >= Qualifiers::Error()) {
      qualifiers |= Qualifiers::Error();
      continue;
    }

    if (qt.type() != Type) {
      qualifiers |= Qualifiers::Error();
      tv.ConsumeDiagnostic(NonTypeFunctionOutput{.view = out->range()});
    }
  }

  co_return tv.TypeOf(node, QualifiedType(Type, qualifiers));
}

}  // namespace semantic_analysis
