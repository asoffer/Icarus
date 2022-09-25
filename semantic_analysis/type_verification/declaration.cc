#include "ast/ast.h"
#include "compiler/common_diagnostics.h"
#include "semantic_analysis/type_verification/verify.h"
#include "semantic_analysis/type_system.h"

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
std::optional<core::Type> EvaluateAsType(Context &context,
                                         TypeSystem &type_system,
                                         ast::Expression const *expr) {
  auto qt = context.qualified_type(expr);
  bool has_error = (qt.qualifiers() >= Qualifiers::Error());
  ASSERT(has_error == false);
  return core::Type(core::PointerType(type_system, I(64)));
}

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Declaration const *node) {
  absl::Span<QualifiedType const> init_val_qts(nullptr, 0);
  absl::Span<QualifiedType const> type_expr_qts(nullptr, 0);

  if (auto *e = node->init_val()) { init_val_qts = co_await VerifyTypeOf(e); }
  if (auto *e = node->type_expr()) { type_expr_qts = co_await VerifyTypeOf(e); }

  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      // Syntactically: `var: T`
      if (type_expr_qts.size() != 1) { NOT_YET("Log an error"); }
      auto type_expr_qt = type_expr_qts[0];
      if (type_expr_qt != Constant(Type)) { NOT_YET("Log an error"); }
      std::optional t =
          EvaluateAsType(tv.context(), tv.type_system(), node->type_expr());
      if (not t) {
        tv.complete_verification(node, Error());
        co_return;
      }

      // TODO: If it's a local variable, the type needs to be default
      // initializable. If it's a parameter the type does not need to be default
      // initializable.
      // bool is_parameter = (node->flags() & ast::Declaration::f_IsFnParam);
      // bool is_default_initializable = t->get()->IsDefaultInitializable();
      // if (not is_parameter and not is_default_initializable) {
      //   tv.ConsumeDiagnostic(NoDefaultValue{
      //       .type = TypeForDiagnostic(node, tv.context()),
      //       .view = node->range(),
      //   });
      // }

      QualifiedType qt(*t, Qualifiers());
      if (node->flags() & ast::Declaration::f_IsConst) { qt = Constant(qt); }
      for (auto const &id : node->ids()) { tv.complete_verification(&id, qt); }
      tv.complete_verification(node, qt);
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
