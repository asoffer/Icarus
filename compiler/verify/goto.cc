#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct NonBooleanConditionalGoto {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-boolean-conditional-goto";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Condition in a `goto` expression must be a boolean, "
                         "but encountered a %s",
                         type),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  frontend::SourceRange range;
  type::Type type;
};

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(ast::ConditionalGoto const *node) {
  auto qt = VerifyType(node->condition())[0];

  if (qt.type() != type::Bool) {
    diag().Consume(NonBooleanConditionalGoto{
        .range = node->condition()->range(),
        .type  = qt.type(),
    });
  }

  for (auto const &option : node->true_options()) {
    for (auto const &expr : option.args()) { VerifyType(expr.get()); }
  }
  for (auto const &option : node->false_options()) {
    for (auto const &expr : option.args()) { VerifyType(expr.get()); }
  }

  return {};
}

absl::Span<type::QualType const> Compiler::VerifyType(ast::UnconditionalGoto const *node) {
  for (auto const &option : node->options()) {
    for (auto const &expr : option.args()) { VerifyType(expr.get()); }
  }
  return {};
}

}  // namespace compiler
