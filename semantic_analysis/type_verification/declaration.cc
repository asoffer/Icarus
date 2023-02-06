#include "ast/ast.h"
#include "compiler/common_diagnostics.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/casting.h"
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

struct NoValidCast {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-valid-cast";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No valid cast from a value of type `%s` to a value "
                         "of type `%s`.",
                         from, to),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
  std::string from;
  std::string to;
};

struct OutOfBoundsConstantInteger {
  static constexpr std::string_view kCategory = "cast-error";
  static constexpr std::string_view kName = "out-of-bounds-constant-integer";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot cast the integer value %s to the type `%s` "
                         "because the value would be out of range.",
                         value, type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
  std::string_view value;
  std::string type;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Declaration const *node) {
  std::span<QualifiedType const> initial_value_qts;
  std::span<QualifiedType const> type_expr_qts;

  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      // Syntactically: `var: T`, or `var :: T`
      type_expr_qts = co_await VerifyTypeOf(node->type_expr());
      if (type_expr_qts.size() != 1) { NOT_YET("Log an error"); }
      auto type_expr_qt = type_expr_qts[0];
      if (type_expr_qt != Constant(Type)) {
        tv.ConsumeDiagnostic(NonConstantTypeInDeclaration{
            .view = node->type_expr()->range(),
        });
        co_return tv.TypeOf(node, Error());
      }

      // TODO: If it's a local variable, the type needs to be default
      // initializable. If it's a parameter the type does not need to be default
      // initializable.
      // bool is_parameter = (node->flags() & ast::Declaration::f_IsFnParam);
      // bool is_default_initializable = t->get()->IsDefaultInitializable();
      // if (not is_parameter and not is_default_initializable) {
      //   tv.ConsumeDiagnostic(NoDefaultValue{
      //       .type = tv.TypeForDiagnostic(node),
      //       .view = node->range(),
      //   });
      // }

      QualifiedType qt(tv.EvaluateAs<core::Type>(node->type_expr()));
      if (node->flags() & ast::Declaration::f_IsConst) { qt = Constant(qt); }
      co_return tv.TypeOf(node, qt);
    } break;
    case ast::Declaration::kInferred: {
      // Syntactically: `var := value`, or `var ::= value`
      std::span parameters = co_await VerifyParametersOf(node->init_val());
      if (parameters.data() != nullptr) {
        ASSERT(parameters.size() == 1);
        co_yield tv.ParametersOf(node, parameters[0]);
      }

      initial_value_qts = co_await VerifyTypeOf(node->init_val());
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
      co_return tv.TypeOf(node, qt);
    } break;
    case ast::Declaration::kCustomInit: {
      // Syntactically: `var: T = value`, or `var :: T = value`
      type_expr_qts = co_await VerifyTypeOf(node->type_expr());
      if (type_expr_qts.size() != 1) { NOT_YET("Log an error"); }
      auto type_expr_qt = type_expr_qts[0];
      if (type_expr_qt != Constant(Type)) {
        tv.ConsumeDiagnostic(NonConstantTypeInDeclaration{
            .view = node->type_expr()->range(),
        });
        co_return tv.TypeOf(node, Error());
      }

      core::Type t = tv.EvaluateAs<core::Type>(node->type_expr());
      if (node->flags() & ast::Declaration::f_IsConst) {
        co_yield tv.TypeOf(node, Constant(t));
      } else {
        co_yield tv.TypeOf(node, QualifiedType(t));
      }

      std::span parameters = co_await VerifyParametersOf(node->init_val());
      if (parameters.data() != nullptr) {
        ASSERT(parameters.size() == 1);
        co_yield tv.ParametersOf(node, parameters[0]);
      }

      initial_value_qts = co_await VerifyTypeOf(node->init_val());
      if (initial_value_qts.size() != 1) { NOT_YET("Log an error"); }
      QualifiedType init_qt(initial_value_qts[0].type());
      if (node->flags() & ast::Declaration::f_IsConst) {
        if (not(initial_value_qts[0].qualifiers() >= Qualifiers::Constant())) {
          tv.ConsumeDiagnostic(InitializingConstantWithNonConstant{
              .view = node->init_val()->range(),
          });
          init_qt = Error(init_qt);
        }
        init_qt = Constant(init_qt);
      }

      QualifiedType qt(t);
      if (node->flags() & ast::Declaration::f_IsConst) { qt = Constant(qt); }

      if (CanCast(init_qt, t, tv.type_system()) == CastKind::None) {
        co_return tv.TypeOf(node, Error(qt));
      }

      co_return tv.TypeOf(node, qt);
    } break;
    default: NOT_YET(node->DebugString());
  }
}

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Declaration::Id const *node) {
  std::span parameters = co_await VerifyParametersOf(&node->declaration());
  if (parameters.data() != nullptr) {
    ASSERT(parameters.size() == node->declaration().ids().size());
    co_yield tv.ParametersOf(node, parameters[node->index()]);
  }
  std::span qts = co_await VerifyTypeOf(&node->declaration());
  co_return tv.TypeOf(node, qts[node->index()]);
}

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::BindingDeclaration const *node) {
  UNREACHABLE();
}

}  // namespace semantic_analysis
