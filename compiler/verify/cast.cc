#include "type/cast.h"

#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/common.h"
#include "compiler/verify/verify.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct CastToNonConstantType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "cast-to-non-constant-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Cannot cast to a type which is not declared constant."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::Cast const *node) {
  auto expr_qt = VerifyType(node->expr())[0];
  auto type_qt = VerifyType(node->type())[0];
  if (not expr_qt.ok() or not type_qt.ok()) {
    return context().set_qual_type(node, type::QualType::Error());
  }

  if (type_qt.type() != type::Type_) {
    diag().Consume(NotAType{
        .view = node->range(),
        .type = TypeForDiagnostic(node->type(), context()),
    });
    return context().set_qual_type(node, type::QualType::Error());
  }
  if (not type_qt.constant()) {
    diag().Consume(CastToNonConstantType{.view = node->range()});
    return context().set_qual_type(node, type::QualType::Error());
  }

  ASSIGN_OR(return context().set_qual_type(node, type::QualType::Error()),  //
                   auto t, EvaluateOrDiagnoseAs<type::Type>(node->type()));
  type::QualType qt(t, expr_qt.quals() & ~type::Quals::Buf());
  if (not type::CanCastExplicitly(expr_qt.type(), t)) {
    diag().Consume(InvalidCast{
        .from = TypeForDiagnostic(node->expr(), context()),
        .to   = TypeForDiagnostic(node, context()),
        .view = node->range(),
    });
    qt.MarkError();
  }

  return context().set_qual_type(node, qt);
}

}  // namespace compiler
