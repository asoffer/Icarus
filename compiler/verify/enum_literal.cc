#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

struct NonConstantEnumerator {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-enumerator";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Values for enumerators must be declared as constants."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};
struct NonIntegralEnumerator {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-integral-enumerator";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Values for enumerators must be integers, but we "
                         "found an enumerator of type `%s`.",
                         type),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  type::Type type;
};

WorkItem::Result Compiler::VerifyBody(ast::EnumLiteral const *node) {
  bool success = true;
  for (auto const &[name, value] : node->specified_values()) {
    auto qts = VerifyType(value.get());
    if (not(qts[0].quals() >= type::Quals::Const())) {
      success = false;
      diag().Consume(NonConstantEnumerator{.range = value->range()});
    }
    if (not type::IsIntegral(qts[0].type())) {
      success = false;
      diag().Consume(NonIntegralEnumerator{
          .range = value->range(),
          .type  = qts[0].type(),
      });
    }
  }
  return success ? WorkItem::Result::Success : WorkItem::Result::Failure;
}

absl::Span<type::QualType const> Compiler::VerifyType(ast::EnumLiteral const *node) {
  LOG("compile-work-queue", "Request work enum: %p", node);
  state_.work_queue.Enqueue({
      .kind      = WorkItem::Kind::VerifyEnumBody,
      .node      = node,
      .resources = resources_,
  });
  return context().set_qual_type(node, type::QualType::Constant(type::Type_));
}

}  // namespace compiler
