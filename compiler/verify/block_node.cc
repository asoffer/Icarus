#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/context.h"

namespace compiler {
namespace {

struct NoBlockWithName {
  static constexpr std::string_view kCategory = "reference-error";
  static constexpr std::string_view kName     = "no-block-with-name";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No block on this scope with the name `%s`.", name),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  std::string name;
  frontend::SourceRange range;
};

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(ast::BlockNode const *node) {
  LOG("BlockNode", "Verifying %s", node->DebugString());
  auto qt = type::QualType::Constant(type::Void);

  auto const *scope_node = node->parent();
  if (not scope_node) {
    NOT_YET(
        "Currently blocks must have parents. Decide if this is always going to "
        "be the case and make it return a const-reference, or support blocks "
        "without parents.");
  }

  std::optional<ir::Scope> scope =
      EvaluateOrDiagnoseAs<ir::Scope>(scope_node->name());
  if (not scope) {
    qt.MarkError();
    return context().set_qual_type(node, qt);
  }

  // TODO: Verify that the block's name makes sense.

  for (auto &param : node->params()) {
    if (not VerifyType(param.value.get())[0].ok()) { qt.MarkError(); }
  }

  if (not qt.HasErrorMark()) {
    for (auto *stmt : node->stmts()) {
      absl::Span<type::QualType const> qts = VerifyType(stmt);
      if (qts.size() == 1 and not qts[0].ok()) { qt.MarkError(); }
    }
  }

  return context().set_qual_type(node, qt);
}

}  // namespace compiler
