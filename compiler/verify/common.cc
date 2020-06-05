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

// TODO: Return more information than just "did this fail."
static bool ExtractParams(
    type::Callable const *callable, core::FnArgs<type::QualType> const &args,
    std::vector<std::pair<type::Callable const *, core::Params<type::QualType>>>
        &overload_params) {
  if (auto const *f = callable->if_as<type::Function>()) {
    auto params = MatchArgsToParams(f->params(), args);
    if (params) {
      overload_params.emplace_back(f, *std::move(params));
    } else {
      return false;
    }

  } else if (auto const *os = callable->if_as<type::OverloadSet>()) {
    for (auto const *overload : os->members()) {
      if (not ExtractParams(overload, args, overload_params)) { return false; }
    }
  } else if (auto const *gf = callable->if_as<type::GenericFunction>()) {
    NOT_YET(*gf);
  } else {
    UNREACHABLE();
  }
  return true;
}

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
base::expected<type::QualType, std::vector<core::FnArgs<type::QualType>>>
Compiler::VerifyCall(
    absl::flat_hash_map<ast::Expression const *, type::Callable const *> const
        &overload_map,
    core::FnArgs<type::Typed<ir::Value>> const &args) {
  // TODO: Take a type::Typed<ir::Value> instead.
  auto args_qt = args.Transform([](auto const &typed_value) {
    return typed_value->empty()
               ? type::QualType::NonConstant(typed_value.type())
               : type::QualType::Constant(typed_value.type());
  });

  std::vector<core::FnArgs<type::QualType>> arg_fails;

  std::vector<std::vector<type::Type const *>> return_types;
  for (auto const &expansion : ExpandedFnArgs(args_qt)) {
    DEBUG_LOG()("Expansion!", overload_map.size());
    for (auto const &[callee, callable_type] : overload_map) {
      DEBUG_LOG()(*callable_type);
      std::vector<
          std::pair<type::Callable const *, core::Params<type::QualType>>>
          overload_params;
      if (not ExtractParams(callable_type, args_qt, overload_params)) {
        return std::vector<core::FnArgs<type::QualType>>{};
      }
      DEBUG_LOG()(overload_params);

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

      arg_fails.push_back(args_qt);
      return arg_fails;
    next_expansion:;
    }
  }

  return type::QualType(type::MultiVar(return_types),
                        type::Quals::Unqualified());
}

}  // namespace compiler
