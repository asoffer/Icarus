#include "ast/ast.h"
#include "semantic_analysis/type_verification/casting.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct NonTypeInDeclaration {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-type-in-cast";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "The target of a cast must be a type, but you provided a(n) `%s`.",
            type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string type;
  std::string_view view;
};

struct NonConstantTypeInDeclaration {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-type-in-cast";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Non-constant type encountered as target of a cast."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

struct NoViableCast {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-viable-cast";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No viable cast from %s to %s", from_type, to_type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string from_type;
  std::string to_type;
  std::string_view view;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(ast::Cast const* node) {
  std::span type_qts = co_await VerifyTypeOf(node->type());
  if (type_qts.size() != 1) { NTH_UNIMPLEMENTED("Log an error"); }

  if (type_qts[0].type() != Type) {
    ConsumeDiagnostic(NonTypeInDeclaration{
        .type = "TODO",
        .view = node->type()->range(),
    });
    co_return TypeOf(node, Error());
  } else if (not(type_qts[0].qualifiers() >= Qualifiers::Constant())) {
    ConsumeDiagnostic(NonConstantTypeInDeclaration{
        .view = node->type()->range(),
    });
    co_return TypeOf(node, Error());
  }

  std::span expr_qts = co_await VerifyTypeOf(node->expr());
  if (expr_qts.size() != 1) { NTH_UNIMPLEMENTED("Log an error"); }
  QualifiedType qt = expr_qts[0];

  core::Type t = EvaluateAs<core::Type>(node->type());
  switch (CanCast(qt, t)) {
    case CastKind::None:
      ConsumeDiagnostic(NoViableCast{
          .from_type = TypeForDiagnostic(*node->expr()),
          .to_type   = ExpressionForDiagnostic(*node->type()),
          .view      = node->type()->range(),
      });

      co_return TypeOf(node, Error(QualifiedType(
                                 t, qt.qualifiers() & Qualifiers::Constant())));
    case CastKind::Explicit:
    case CastKind::Implicit:
      co_return TypeOf(
          node, QualifiedType(t, qt.qualifiers() & (Qualifiers::Constant() |
                                                    Qualifiers::Error())));
    case CastKind::InPlace:
      co_return TypeOf(
          node, QualifiedType(t, qt.qualifiers() & (Qualifiers::Constant() |
                                                    Qualifiers::Buffer() |
                                                    Qualifiers::Error())));
  }
}

}  // namespace semantic_analysis