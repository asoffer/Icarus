#include "ast/ast.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {

VerificationTask TypeVerifier::VerifyType(
    ast::ShortFunctionLiteral const* node) {
  core::Parameters<core::Type> parameters;
  bool has_error = false;
  for (auto const& parameter : node->parameters()) {
    std::span parameter_qts = co_await VerifyTypeOf(&parameter.value);
    if (parameter_qts.size() != 1) {
      NTH_UNIMPLEMENTED("Log an error: {}") <<= {parameter_qts.size()};
      has_error = true;
    } else if (parameter_qts[0].qualifiers() >= Qualifiers::Error()) {
      has_error = true;
    }

    parameters.append(parameter.name, parameter_qts[0].type(), parameter.flags);
  }

  core::ParameterType parameter_type(GlobalTypeSystem, parameters);
  absl::flat_hash_map<core::ParameterType, Context::CallableIdentifier>
      parameter_types{{parameter_type, Context::CallableIdentifier(node)}};
  co_yield ParametersOf(node, std::move(parameter_types));

  if (has_error) { co_return TypeOf(node, Error()); }

  std::span body_qualified_types = co_await VerifyTypeOf(node->body());

  std::vector<core::Type> body_types;
  body_types.reserve(body_qualified_types.size());
  for (auto const& qt : body_qualified_types) {
    if (qt.qualifiers() >= Qualifiers::Error()) {
      has_error = true;
    } else {
      body_types.push_back(qt.type());
    }
  }

  co_return TypeOf(node,
                   Constant(core::FunctionType(GlobalTypeSystem, parameter_type,
                                               std::move(body_types))));
}

}  // namespace semantic_analysis
