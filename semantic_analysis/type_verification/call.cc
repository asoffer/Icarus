#include "core/call.h"

#include "ast/ast.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct UncallableWithArguments {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncallable-with-arguments";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage();
  }

  core::Arguments<std::string> arguments;
  std::string_view view;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Call const *node) {
  absl::Span<std::pair<core::ParameterType, Context::CallableIdentifier> const>
      callee_parameter_types = co_await VerifyParametersOf(node->callee());

  core::Arguments<QualifiedType> arguments;
  bool has_error = false;
  for (auto const &argument : node->arguments()) {
    absl::Span argument_qts = co_await VerifyTypeOf(&argument.expr());
    if (argument_qts.size() != 1) {
      NOT_YET("Log an error");
      has_error = true;
    } else if (argument_qts[0].qualifiers() >= Qualifiers::Error()) {
      has_error = true;
    } else {
      if (argument.named()) {
        arguments.named_emplace(argument.name(), argument_qts[0]);
      } else {
        arguments.pos_emplace(argument_qts[0]);
      }
    }
  }

  if (has_error) { co_return tv.TypeOf(node, Error()); }

  std::vector<
      std::pair<core::ParameterType, Context::CallableIdentifier> const *>
      parameter_types;
  for (auto const &callee_parameter_type : callee_parameter_types) {
    auto callability_result = core::Callability(
        callee_parameter_type.first.value(), arguments,
        [&](QualifiedType argument_type, core::Type parameter_type) {
          // TODO: Handle implicit conversions.
          return argument_type.type() == parameter_type;
        });
    if (not callability_result.ok()) { continue; }
    parameter_types.push_back(&callee_parameter_type);
  }
  switch (parameter_types.size()) {
    case 0:
      tv.ConsumeDiagnostic(UncallableWithArguments{});
      co_return tv.TypeOf(node, Error());
    case 1: {
      auto const &[parameter_type, callable_identifier] = *parameter_types[0];
      absl::Span callee_qts =
          co_await VerifyTypeOf(&callable_identifier.expression());
      if (callee_qts.size() != 1) { NOT_YET(); }
      absl::Span<core::Type const> return_types =
          callee_qts[0]
              .type()
              .get<core::FunctionType>(tv.type_system())
              .returns();
      std::vector<QualifiedType> qts;
      qts.reserve(return_types.size());
      for (core::Type t : return_types) { qts.emplace_back(t); }
      co_return tv.TypeOf(node, std::move(qts));
    }
    default: NOT_YET("Log an error");
  }
}

}  // namespace semantic_analysis
