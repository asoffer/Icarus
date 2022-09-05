#include "ast/ast.h"
#include "compiler/common_diagnostics.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/pointer.h"
#include "type/qual_type.h"

namespace semantic_analysis {
namespace {

struct NoDefaultValue {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-default-value";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("There is no default value for the type `%s`.", type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string type;
  std::string_view view;
};

// TODO: Make this useful.
std::optional<type::Type> EvaluateAsType(compiler::Context &context,
                                         ast::Expression const &expr) {
  ASSERT(context.qual_types(&expr).size() == 1);
  ASSERT(context.qual_types(&expr)[0] != type::QualType::Error());
  ASSERT(context.qual_types(&expr)[0].HasErrorMark() == false);
  return type::Ptr(type::I64);
}

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Declaration const *node) {
  absl::Span<type::QualType const> init_val_qts(nullptr, 0);
  absl::Span<type::QualType const> type_expr_qts(nullptr, 0);

  if (auto *e = node->init_val()) { init_val_qts = co_await VerifyTypeOf(e); }
  if (auto *e = node->type_expr()) { type_expr_qts = co_await VerifyTypeOf(e); }

  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      if (type_expr_qts.size() != 1) { NOT_YET("Log an error"); }
      auto type_expr_qt = type_expr_qts[0];
      if (type_expr_qt != type::QualType::Constant(type::Type_)) {
        NOT_YET("Log an error");
      }
      std::optional t = EvaluateAsType(tv.context(), *node->type_expr());
      if (not t) {
        tv.complete_verification(node, type::QualType::Error());
        co_return;
      }

      bool is_parameter = (node->flags() & ast::Declaration::f_IsFnParam);
      bool is_default_initializable = t->get()->IsDefaultInitializable();
      if (not is_parameter and not is_default_initializable) {
        tv.ConsumeDiagnostic(NoDefaultValue{
            .type = TypeForDiagnostic(node, tv.context()),
            .view = node->range(),
        });
      }

      for (auto const &id : node->ids()) {
        tv.complete_verification(&id, type::QualType::Constant(*t));
      }
      tv.complete_verification(node, type::QualType::Constant(*t));
    } break;
    default: NOT_YET(node->DebugString());
  }
}

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Declaration::Id const *node) {
  return VerifyType(tv, &node->declaration());
}

VerificationTask TypeVerifier::VerifyType(TypeVerifier & tv,
                                          ast::BindingDeclaration const *node) {
  UNREACHABLE();
}

}  // namespace semantic_analysis
