#include "ast/ast.h"
#include "compiler/common_diagnostics.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct NonConstantConditionError {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-condition-error";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("A #{const} if statement requires that the condition "
                         "be a compile-time constant"),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

struct NonBooleanCondition {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-boolean-condition";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("If statements require the condition to be of type "
                         "`bool`, but you provided a value of type `%s`.",
                         type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
  std::string type;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(ast::IfStmt const *node) {
  std::span condition_qts = co_await VerifyTypeOf(&node->condition());
  switch (condition_qts.size()) {
    case 0: NOT_YET("Log an error");
    case 1: {
      bool condition_has_error = false;
      QualifiedType qt         = condition_qts[0];
      if (qt.type() != Bool) {
        ConsumeDiagnostic(NonBooleanCondition{
            .view = node->range(),
            .type = TypeForDiagnostic(node->condition()),
        });
        condition_has_error = true;
      }
      condition_has_error |= (qt.qualifiers() >= Qualifiers::Error());

      std::vector<QualifiedType> yielded_qts;

      bool marked_const = node->hashtags.contains(data_types::Hashtag::Const);

      if (marked_const) {
        // If the condition has an error, and it is a `#{const}` if-statement,
        // we can't know which side is supposed to be type-checked, but we know
        // only one side is even expected to be correct. Rather than emitting
        // too many errors by checking both sides, guaranteeing irrelevant
        // information, we check neither side. An error was already emitted for
        // the condition and the user will be able to see further errors by
        // fixing the bug in the conditional statement.
        if (not condition_has_error) {
          if (qt.qualifiers() >= Qualifiers::Constant()) {
            if (EvaluateAs<bool>(&node->condition())) {
              for (auto const *stmt : node->true_block()) {
                co_await VerifyTypeOf(stmt);
              }
            } else if (node->has_false_block()) {
              for (auto const *stmt : node->false_block()) {
                co_await VerifyTypeOf(stmt);
              }
            }
          } else {
            ConsumeDiagnostic(NonConstantConditionError{
                .view = node->condition().range(),
            });
          }
        }
      } else {
        // TODO: Write yielded_qts correctly.
        for (auto const *stmt : node->true_block()) {
          co_await VerifyTypeOf(stmt);
        }
        for (auto const *stmt : node->false_block()) {
          co_await VerifyTypeOf(stmt);
        }
      }

      co_return TypeOf(node, std::move(yielded_qts));
    } break;
    default: NOT_YET("Log an error");
  }
}

}  // namespace semantic_analysis
