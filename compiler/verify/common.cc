#include <optional>
#include <string_view>

#include "compiler/compiler.h"
#include "compiler/dispatch/match.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "core/call.h"
#include "core/fn_args.h"
#include "diagnostic/errors.h"
#include "ir/value/value.h"
#include "type/callable.h"
#include "type/overload_set.h"
#include "type/typed_value.h"
#include "type/variant.h"

namespace compiler {
namespace {

type::Typed<ir::Value> EvaluateIfConstant(Compiler &c,
                                          ast::Expression const *expr,
                                          type::QualType qt) {
  if (qt.constant()) {
    DEBUG_LOG("EvaluateIfConstant")
    ("Evaluating constant: ", expr->DebugString());
    auto maybe_val = c.Evaluate(type::Typed(expr, qt.type()));
    if (maybe_val) { return type::Typed<ir::Value>(*maybe_val, qt.type()); }
    c.diag().Consume(diagnostic::EvaluationFailure{
        .failure = maybe_val.error(),
        .range   = expr->range(),
    });
  }
  return type::Typed<ir::Value>(ir::Value(), qt.type());
}

}  // namespace

std::optional<core::FnArgs<type::Typed<ir::Value>>> Compiler::VerifyFnArgs(
    core::FnArgs<ast::Expression const *> const &args) {
  bool err      = false;
  auto arg_vals = args.Transform([&](ast::Expression const *expr) {
    auto expr_qual_type = VerifyType(expr);
    err |= not expr_qual_type.ok();
    if (err) {
      DEBUG_LOG("VerifyFnArgs")("Error with: ", expr->DebugString());
      return type::Typed<ir::Value>(ir::Value(), nullptr);
    }
    DEBUG_LOG("VerifyFnArgs")("constant: ", expr->DebugString());
    return EvaluateIfConstant(*this, expr, expr_qual_type);
  });

  if (err) { return std::nullopt; }
  return arg_vals;
}

// TODO: Support calling with constants.
// TODO: Replace `symbol` with an enum.
type::QualType Compiler::VerifyUnaryOverload(char const *symbol,
                                             ast::Expression const *node,
                                             type::Type const *operand_type) {
  absl::flat_hash_set<type::Callable const *> member_types;

  module::ForEachDeclTowardsRoot(
      node->scope(), symbol, [&](ast::Expression const *expr) {
        ASSIGN_OR(return false, auto qt, qual_type_of(expr));
        // Must be callable because we're looking at overloads for operators
        // which have previously been type-checked to ensure callability.
        auto &c = qt.type()->as<type::Callable>();
        member_types.insert(&c);
        return true;
      });

  if (member_types.empty()) { return type::QualType::Error(); }
  std::vector<type::Typed<ir::Value>> pos_args;
  pos_args.emplace_back(ir::Value(), operand_type);
  return type::QualType(type::MakeOverloadSet(std::move(member_types))
                            ->return_types(core::FnArgs<type::Typed<ir::Value>>(
                                std::move(pos_args), {})),
                        type::Quals::Unqualified());
}

// TODO: Accept frontend::Operator rather than char const*.
// TODO: Support calling with constants.
// TODO: Replace `symbol` with an enum.
type::QualType Compiler::VerifyBinaryOverload(std::string_view symbol,
                                              ast::Expression const *node,
                                              type::Type const *lhs_type,
                                              type::Type const *rhs_type) {
  absl::flat_hash_set<type::Callable const *> member_types;

  module::ForEachDeclTowardsRoot(
      node->scope(), symbol, [&](ast::Expression const *expr) {
        ASSIGN_OR(return false, auto qt, qual_type_of(expr));
        // Must be callable because we're looking at overloads for operators
        // which have previously been type-checked to ensure callability.
        auto &c = qt.type()->as<type::Callable>();
        member_types.insert(&c);
        return true;
      });

  if (member_types.empty()) { return type::QualType::Error(); }
  std::vector<type::Typed<ir::Value>> pos_args;
  pos_args.emplace_back(ir::Value(), lhs_type);
  pos_args.emplace_back(ir::Value(), rhs_type);
  return data().set_qual_type(
      node,
      type::QualType(type::MakeOverloadSet(std::move(member_types))
                         ->return_types(core::FnArgs<type::Typed<ir::Value>>(
                             std::move(pos_args), {})),
                     type::Quals::Unqualified()));
}

namespace {

using CallMatchResult = std::variant<core::Params<type::QualType>,
                                     Compiler::CallError::ErrorReason>;

// Determines which arguments are passed to which parameters. No type-checking
// is done in this phase. Matching arguments to parameters can be done, even on
// generics without any type-checking.
CallMatchResult MatchArgumentsToParameters(
    core::Params<type::QualType> const &params,
    core::FnArgs<type::QualType> const &args) {
  if (args.size() > params.size()) {
    return Compiler::CallError::TooManyArguments{
        .num_provided     = args.size(),
        .max_num_accepted = params.size(),
    };
  }

  core::Params<type::QualType> matched_params;
  // Match positional arguments
  for (size_t i = 0; i < args.pos().size(); ++i) {
    auto const &param = params[i];
    matched_params.append(param.name, args[i], param.flags);
  }

  for (size_t i = args.pos().size(); i < params.size(); ++i) {
    auto const &param = params[i];
    DEBUG_LOG("match")
    ("Matching param in position ", i, "(name = ", param.name, ")");
    if (auto *result = args.at_or_null(param.name)) {
      matched_params.append(param.name, *result, param.flags);
    } else {
      // No argument provided by that name? This could be because we have
      // default parameters or an empty variadic pack.
      // TODO: Handle variadic packs.
      if (param.flags & core::HAS_DEFAULT) {
        matched_params.append(param);
      } else {
        return Compiler::CallError::MissingNonDefaultableArgument{
            .name = param.name,
        };
      }
    }
  }
  return matched_params;
}


// TODO: Return more information than just "did this fail."
std::optional<Compiler::CallError::ErrorReason> ExtractParams(
    type::Callable const *callable, core::FnArgs<type::QualType> const &args,
    std::vector<std::pair<type::Callable const *, core::Params<type::QualType>>>
        &overload_params) {
  if (auto const *f = callable->if_as<type::Function>()) {
    auto result = MatchArgumentsToParameters(f->params(), args);
    if (auto *params = std::get_if<core::Params<type::QualType>>(&result)) {
      overload_params.emplace_back(f, std::move(*params));
    } else {
      return std::get<Compiler::CallError::ErrorReason>(std::move(result));
    }

  } else if (auto const *os = callable->if_as<type::OverloadSet>()) {
    for (auto const *overload : os->members()) {
      auto maybe_error = ExtractParams(overload, args, overload_params);
      if (maybe_error.has_value()) {
        // TODO: Show errors.
        // error.reasons.emplace(overload, *std::move(maybe_error));
        continue;
      }
    }
    if (overload_params.empty()) { return std::nullopt; }
  } else if (auto const *gf = callable->if_as<type::GenericFunction>()) {
    NOT_YET(*gf);
  } else {
    UNREACHABLE();
  }
  return std::nullopt;
}

}  // namespace

std::pair<type::QualType,
          absl::flat_hash_map<ast::Expression const *, type::Callable const *>>
Compiler::VerifyCallee(ast::Expression const *callee,
                       core::FnArgs<type::Typed<ir::Value>> const &args) {
  using return_type =
      std::pair<type::QualType, absl::flat_hash_map<ast::Expression const *,
                                                    type::Callable const *>>;
  ASSIGN_OR(return return_type(type::QualType::Error(), {}),  //
                   auto qt, VerifyType(callee));

  ASSIGN_OR(return return_type(qt, {}),  //
                   auto const &callable, qt.type()->if_as<type::Callable>());
  return return_type(qt, {{callee, &callable}});
}

// TODO: Build a data structure that holds information about which overloads did
// not match and why, as well as which expanded argument sets were not covered
// by the parameters.
base::expected<type::QualType, Compiler::CallError> Compiler::VerifyCall(
    absl::flat_hash_map<ast::Expression const *, type::Callable const *> const
        &overload_map,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  // TODO: Take a type::Typed<ir::Value> instead.
  auto args_qt = args.Transform([](auto const &typed_value) {
    return typed_value->empty()
               ? type::QualType::NonConstant(typed_value.type())
               : type::QualType::Constant(typed_value.type());
  });

  // TODO: Expansion is relevant too.
  CallError errors;
  std::vector<std::vector<type::Type const *>> return_types;
  for (auto const &expansion : ExpandedFnArgs(args_qt)) {
    for (auto const &[callee, callable_type] : overload_map) {
      std::vector<
          std::pair<type::Callable const *, core::Params<type::QualType>>>
          overload_params;
      auto maybe_error = ExtractParams(callable_type, args_qt, overload_params);
      if (maybe_error.has_value()) {
        errors.reasons.emplace(callee, std::move(*maybe_error));
        return errors;
      }

      for (auto const &[callable_type, param] : overload_params) {
        // TODO: Assuming this is unambiguously callable is a bit of a stretch.
        if (core::IsCallable(core::ParamsRef(param), expansion,
                             [](type::Type const *arg, type::QualType param) {
                               return type::CanCast(arg, param.type());
                             })) {
          return_types.push_back(callable_type->return_types(args));
          goto next_expansion;
        }
      }

      return errors;
    next_expansion:;
    }
  }

  return type::QualType(type::MultiVar(return_types),
                        type::Quals::Unqualified());
}

}  // namespace compiler
