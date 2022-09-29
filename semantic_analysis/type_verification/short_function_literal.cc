#include "ast/ast.h"
#include "compiler/context.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/function.h"
#include "type/qual_type.h"

namespace semantic_analysis {

VerificationTask TypeVerifier::VerifyType(
    TypeVerifier& tv, ast::ShortFunctionLiteral const* node) {
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
  std::vector<std::pair<core::ParameterType, Context::CallableIdentifier>> parameter_types;
  parameter_types.emplace_back(parameter_type,
                               Context::CallableIdentifier(node));
  tv.complete_parameters(node, std::move(parameter_types));

  if (has_error) {
    tv.complete_verification(node, Error());
    co_return;
  }

  absl::Span body_qualified_types = co_await VerifyTypeOf(node->body());

  std::vector<core::Type> body_types;
  body_types.reserve(body_qualified_types.size());
  for (auto const& qt : body_qualified_types) {
    if (qt.qualifiers() >= Qualifiers::Error()) {
      has_error = true;
    } else {
      body_types.push_back(qt.type());
    }
  }

  tv.complete_verification(
      node, Constant(core::FunctionType(tv.type_system(), parameter_type,
                                        std::move(body_types))));
}

}  // namespace semantic_analysis

