#include <string_view>

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "diagnostic/message.h"
#include "type/qual_type.h"

namespace compiler {

struct NonTypeFunctionInput {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-type-function-input";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "The specified input type for a function must be a type."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }
  frontend::SourceRange range;
};

struct NonTypeFunctionOutput {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-type-function-output";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "The specified return type for a function must be a type."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }
  frontend::SourceRange range;
};

type::QualType Compiler::VerifyType(ast::FunctionType const *node) {
  type::Type const *t = type::Type_;
  type::Quals quals   = type::Quals::Const();

  for (auto const *p : node->params()) {
    auto qt = VerifyType(p);
    if (not qt) {
      t = nullptr;
      continue;
    }

    if (not p->is<ast::Declaration>()) { quals &= qt.quals(); }
    if (not p->is<ast::Declaration>() and qt.type() != type::Type_) {
      t = nullptr;
      diag().Consume(NonTypeFunctionInput{.range = p->range()});
    }
  }

  for (auto const *p : node->outputs()) {
    auto qt = VerifyType(p);
    if (not qt) {
      t = nullptr;
      continue;
    }
    quals &= qt.quals();
    if (qt.type() != type::Type_) {
      t = nullptr;
      diag().Consume(NonTypeFunctionOutput{.range = p->range()});
    }
  }

  type::QualType qt(type::Type_, quals);
  if (t == nullptr) { qt.MarkError(); }
  return data().set_qual_type(node, qt);
}

}  // namespace compiler
