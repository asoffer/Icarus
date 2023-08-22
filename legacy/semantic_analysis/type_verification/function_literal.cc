#include "ast/ast.h"
#include "semantic_analysis/type_verification/casting.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {

VerificationTask TypeVerifier::VerifyType(ast::FunctionLiteral const* node) {
  core::Parameters<core::Type> parameters;
  bool has_error = false;
  for (auto const& parameter : node->parameters()) {
    NTH_ASSERT(parameter.value.ids().size() == 1);
    std::span parameter_qts = co_await VerifyTypeOf(&parameter.value.ids()[0]);
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

  if (std::optional outputs = node->outputs()) {
    std::vector<core::Type> return_types;
    for (auto const* output : *outputs) {
      std::span qualified_types = co_await VerifyTypeOf(output);
      for (QualifiedType qt : qualified_types) {
        if (not(qt.qualifiers() >= Qualifiers::Constant())) {
          NTH_UNIMPLEMENTED("Log an error.");
          has_error = true;
        } else if (qt.type() != Type) {
          NTH_UNIMPLEMENTED("Log an error.");
          has_error = true;
        }
      }
    }
    for (auto const* output : *outputs) {
      return_types.push_back(EvaluateAs<core::Type>(output));
    }

    std::span<core::Type const> specified_return_types = return_types;

    co_yield TypeOf(node, Constant(core::FunctionType(
                              GlobalTypeSystem, parameter_type, return_types)));

    bool last_was_noreturn = false;
    for (auto const* stmt : node->stmts()) {
      if (last_was_noreturn) {
        last_was_noreturn = false;
        ConsumeDiagnostic(UnreachableStatement{.view = stmt->range()});
      }
      std::span qts     = co_await VerifyTypeOf(stmt);
      last_was_noreturn = (qts.size() == 1 and qts[0].type() == NoReturn);
    }

    for (auto const* return_stmt : node->returns()) {
      std::span returned_types = context().return_types(return_stmt);

      if (specified_return_types.size() != returned_types.size()) { NTH_UNIMPLEMENTED(); }
      for (size_t i = 0; i < specified_return_types.size(); ++i) {
        switch (CanCast(returned_types[i], specified_return_types[i])) {
          case CastKind::None:
          case CastKind::Explicit:
            NTH_UNIMPLEMENTED("type = {}, qt = {}, node = {}") <<=
                {DebugType(specified_return_types[i]),
                 DebugQualifiedType(returned_types[i]), node->DebugString()};

          case CastKind::Implicit:
          case CastKind::InPlace: continue;
        }
      }
    }

  } else {
    for (auto const* stmt : node->stmts()) { co_await VerifyTypeOf(stmt); }

    std::vector<std::span<QualifiedType const>> returns;
    returns.reserve(node->returns().size());
    for (auto const* return_stmt : node->returns()) {
      returns.push_back(context().return_types(return_stmt));
    }
    switch (returns.size()) {
      case 0:
        co_yield TypeOf(node, Constant(core::FunctionType(GlobalTypeSystem,
                                                          parameter_type, {})));
        break;
      case 1: {
        std::vector<core::Type> return_types;
        return_types.reserve(returns[0].size());
        for (QualifiedType qt : returns[0]) {
          return_types.push_back(qt.type());
        }
        co_yield TypeOf(
            node, Constant(core::FunctionType(GlobalTypeSystem, parameter_type,
                                              return_types)));
        break;
      }
      default: NTH_UNIMPLEMENTED();
    }
  }

  co_return Completed(node);
}

}  // namespace semantic_analysis