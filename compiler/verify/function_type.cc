#include <string_view>

#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/verify/verify.h"
#include "diagnostic/message.h"
#include "type/qual_type.h"

namespace compiler {

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

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::FunctionType const *node) {
  type::Type t      = type::Type_;
  type::Quals quals = type::Quals::Const();

  for (auto const *p : node->parameters()) {
    auto qt = VerifyType(p)[0];
    if (not qt) {
      t = nullptr;
      continue;
    }

    if (not p->is<ast::Declaration>()) { quals &= qt.quals(); }
    if (not p->is<ast::Declaration>() and qt.type() != type::Type_) {
      t = nullptr;
      diag().Consume(NonTypeFunctionInput{.view = p->range()});
    }
  }

  for (auto const *p : node->outputs()) {
    auto qt = VerifyType(p)[0];
    if (not qt) {
      t = nullptr;
      continue;
    }
    quals &= qt.quals();
    if (qt.type() != type::Type_) {
      t = nullptr;
      diag().Consume(NonTypeFunctionOutput{.view = p->range()});
    }
  }

  type::QualType qt(type::Type_, quals);
  if (not t.valid()) { qt.MarkError(); }
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
