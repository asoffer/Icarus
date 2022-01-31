#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/verify/verify.h"
#include "compiler/work_item.h"

namespace compiler {

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
  type::Type type;
};

bool BodyVerifier::VerifyBody(ast::EnumLiteral const *node) {
  bool success = true;
  for (auto const &[name, value] : node->specified_values()) {
    auto qts = VerifyType(*this, value.get());
    if (not(qts[0].quals() >= type::Quals::Const())) {
      success = false;
      diag().Consume(NonConstantEnumerator{.view = value->range()});
    }
    if (not type::IsIntegral(qts[0].type())) {
      success = false;
      diag().Consume(NonIntegralEnumerator{
          .view = value.get()->range(),
          .type = qts[0].type(),
      });
    }
  }
  return success;
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::EnumLiteral const *node) {
  LOG("compile-work-queue", "Request work enum: %p", node);
  Enqueue(WorkItem::VerifyBodyOf(node, &context()));
  return context().set_qual_type(node, type::QualType::Constant(type::Type_));
}

}  // namespace compiler
