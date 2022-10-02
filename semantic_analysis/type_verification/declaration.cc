#include "ast/ast.h"
#include "compiler/common_diagnostics.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

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

struct InitializingConstantWithNonConstant {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName =
      "initializing-constant-with-nonconstant";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot initialize a constant from a non-constant."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

struct NonConstantTypeInDeclaration {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "non-constant-type-in-declaration";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Non-constant type encountered in declaration."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Declaration const *node) {
  absl::Span<QualifiedType const> initial_value_qts(nullptr, 0);
  absl::Span<QualifiedType const> type_expr_qts(nullptr, 0);

  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      type_expr_qts = co_await VerifyTypeOf(node->type_expr());
      // Syntactically: `var: T`, or `var :: T`
      if (type_expr_qts.size() != 1) { NOT_YET("Log an error"); }
      auto type_expr_qt = type_expr_qts[0];
      if (type_expr_qt != Constant(Type)) {
        tv.ConsumeDiagnostic(NonConstantTypeInDeclaration{
            .view = node->type_expr()->range(),
        });
        co_return tv.TypeOf(node, Error());
      }

      std::optional t = EvaluateAs<core::Type>(tv.context(), tv.type_system(),
                                               node->type_expr());
      if (not t) { co_return tv.TypeOf(node, Error()); }

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

      QualifiedType qt(*t);
      if (node->flags() & ast::Declaration::f_IsConst) { qt = Constant(qt); }
      for (auto const &id : node->ids()) { co_yield tv.TypeOf(&id, qt); }
      co_return tv.TypeOf(node, qt);
    } break;
    case ast::Declaration::kInferred: {
      if (absl::Span parameters = co_await VerifyParametersOf(node->init_val());
          parameters.data() != nullptr) {
        ASSERT(parameters.size() == node->ids().size());
        size_t i = 0;
        for (auto const &id : node->ids()) {
          co_yield tv.ParametersOf(&id, parameters[i++]);
        }
      }

      initial_value_qts = co_await VerifyTypeOf(node->init_val());
      // Syntactically: `var := value`, or `var ::= value`
      if (initial_value_qts.size() != 1) { NOT_YET("Log an error"); }
      QualifiedType qt(initial_value_qts[0].type());
      if (node->flags() & ast::Declaration::f_IsConst) {
        if (not(initial_value_qts[0].qualifiers() >= Qualifiers::Constant())) {
          tv.ConsumeDiagnostic(InitializingConstantWithNonConstant{
              .view = node->init_val()->range(),
          });
          qt = Error(qt);
        }
        qt = Constant(qt);
      }
      for (auto const &id : node->ids()) { co_yield tv.TypeOf(&id, qt); }
      co_return tv.TypeOf(node, qt);

    } break;
    default: NOT_YET(node->DebugString());
  }
}

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Declaration::Id const *node) {
  return VerifyType(tv, &node->declaration());
}

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::BindingDeclaration const *node) {
  UNREACHABLE();
}

}  // namespace semantic_analysis
