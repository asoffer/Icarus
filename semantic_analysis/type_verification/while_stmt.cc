#include "ast/ast.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct NonBooleanCondition {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-boolean-condition";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("While statements require the condition to be of type "
                         "`bool`, but you provided a value of type `%s`.",
                         type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
  std::string type;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(ast::WhileStmt const *node) {
  std::span condition_qts = co_await VerifyTypeOf(&node->condition());
  switch (condition_qts.size()) {
    case 0: NOT_YET("Log an error");
    case 1: {
      QualifiedType qt = condition_qts[0];
      if (qt.type() != Bool) {
        ConsumeDiagnostic(NonBooleanCondition{
            .view = node->range(),
            .type = TypeForDiagnostic(node->condition()),
        });
      }

      std::vector<QualifiedType> yielded_qts;
      // TODO: Write yielded_qts correctly.
      for (auto const *stmt : node->body()) { co_await VerifyTypeOf(stmt); }
      co_return TypeOf(node, std::move(yielded_qts));
    } break;
    default: NOT_YET("Log an error");
  }
}

}  // namespace semantic_analysis
