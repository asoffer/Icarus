#include "ast/ast.h"
#include "compiler/context.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/function.h"
#include "type/qual_type.h"

namespace semantic_analysis {

VerificationTask TypeVerifier::VerifyType(
    TypeVerifier& tv, ast::ShortFunctionLiteral const* node) {
  core::Parameters<type::QualType> parameters;
  bool has_error = false;
  for (auto const& parameter : node->parameters()) {
    absl::Span<type::QualType const> parameter_qual_types =
        co_await VerifyTypeOf(&parameter.value);
    if (parameter_qual_types.size() == 1 and not parameter_qual_types[0].ok()) {
      has_error = true;
    }

    parameters.append(parameter.name, parameter_qual_types[0], parameter.flags);
  }

  auto const& parameters_ref =
      tv.context().set_parameter_types(node, std::move(parameters));
  tv.set_completed<TypeVerificationPhase::VerifyParameters>(node,
                                                            &parameters_ref);

  if (has_error) {
    tv.context().set_qual_type(node, type::QualType::Error());
    co_return;
  }

  absl::Span body_qual_types = co_await VerifyTypeOf(node->body());

  std::vector<type::Type> body_types;
  body_types.reserve(body_qual_types.size());
  for (auto const& qt : body_qual_types) {
    if (not qt.ok()) {
      has_error = true;
    } else {
      body_types.push_back(qt.type());
    }
  }

  tv.context().set_qual_type(node, type::QualType::Constant(type::Func(
                                       parameters_ref, std::move(body_types))));
  co_return;
}

}  // namespace semantic_analysis

