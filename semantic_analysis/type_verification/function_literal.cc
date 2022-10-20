#include "ast/ast.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {

VerificationTask TypeVerifier::VerifyType(TypeVerifier& tv,
                                          ast::FunctionLiteral const* node) {
  core::Parameters<core::Type> parameters;
  bool has_error = false;
  for (auto const& parameter : node->parameters()) {
    absl::Span parameter_qts = co_await VerifyTypeOf(&parameter.value);
    if (parameter_qts.size() != 1) {
      NOT_YET("Log an error.", parameter_qts.size());
      has_error = true;
    } else if (parameter_qts[0].qualifiers() >= Qualifiers::Error()) {
      has_error = true;
    }

    parameters.append(parameter.name, parameter_qts[0].type(), parameter.flags);
  }

  core::ParameterType parameter_type(tv.type_system(), parameters);
  absl::flat_hash_map<core::ParameterType, Context::CallableIdentifier>
      parameter_types{{parameter_type, Context::CallableIdentifier(node)}};
  co_yield tv.ParametersOf(node, std::move(parameter_types));

  if (has_error) { co_return tv.TypeOf(node, Error()); }

  if (std::optional outputs = node->outputs()) {
    std::vector<core::Type> return_types;
    for (auto const* output : *outputs) {
      absl::Span qualified_types = co_await VerifyTypeOf(output);
      for (QualifiedType qt : qualified_types) {
        if (not(qt.qualifiers() >= Qualifiers::Constant())) {
          NOT_YET("Log an error.");
          has_error = true;
        } else if (qt.type() != Type) {
          NOT_YET("Log an error.");
          has_error = true;
        }
      }
    }
    for (auto const* output : *outputs) {
      return_types.push_back(tv.EvaluateAs<core::Type>(output));
    }

    absl::Span<core::Type const> specified_return_types = return_types;

    co_yield tv.TypeOf(
        node, Constant(core::FunctionType(tv.type_system(), parameter_type,
                                          return_types)));

    for (auto const* stmt : node->stmts()) { co_await VerifyTypeOf(stmt); }

    for (auto const* return_stmt : node->returns()) {
      absl::Span returned_types = tv.context().return_types(return_stmt);

      if (specified_return_types.size() != returned_types.size()) { NOT_YET(); }
      for (size_t i = 0; i < specified_return_types.size(); ++i) {
        if (specified_return_types[i] != returned_types[i]) {
          NOT_YET(DebugType(specified_return_types[i], tv.type_system()),
                  DebugType(returned_types[i], tv.type_system()));
        }
      }
    }

    co_return tv.Completed(node);

  } else {
    NOT_YET();
  }
}

}  // namespace semantic_analysis
