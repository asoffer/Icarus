#include "compiler/verify/common.h"

#include <optional>
#include <string_view>

#include "absl/cleanup/cleanup.h"
#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/module.h"
#include "core/arguments.h"
#include "core/call.h"
#include "type/callable.h"
#include "type/cast.h"
#include "type/overload_set.h"
#include "type/provenance.h"
#include "type/struct.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

bool ValidateCallable(
    type::Callable const *callable,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arguments,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>
        &errors) {
  auto callability = core::Callability(
      callable->params(), arguments,
      [](type::Typed<ir::CompleteResultRef> const &argument,
         type::QualType const &parameter) {
        return type::CanCastImplicitly(argument.type(), parameter.type());
      });
  if (callability.ok()) {
    return true;
  } else {
    errors.emplace(callable, std::move(callability));
    return false;
  }
}

absl::flat_hash_set<type::Typed<CallMetadata::callee_locator_t>> ResolveCall(
    TypeVerifier &tv, CallMetadata const &metadata,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arguments,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>
        &errors) {
  // TODO: if (auto const *r = metadata.resolved()) { return {r}; }

  absl::flat_hash_set<type::Typed<CallMetadata::callee_locator_t>> valid;

  Context const &context_root = tv.context().root();

  for (auto overload : metadata.overloads()) {
    type::Type t;
    if (auto const *e = overload.get_if<ast::Expression>()) {
      t = tv.VerifyType(e)[0].type();
    } else {
      t = overload.get<module::Module::SymbolInformation>()
              ->qualified_type.type();
    }

    if (auto const *callable = t.if_as<type::Callable>()) {
      if (ValidateCallable(callable, arguments, errors)) {
        valid.emplace(overload, callable);
      }
    } else if (auto const *gf = t.if_as<type::Generic<type::Function>>()) {
      auto const *i = gf->Instantiate(tv.work_resources(), arguments);
      if (not i) { continue; }  // TODO: Save an error.
      if (ValidateCallable(i, arguments, errors)) {
        valid.emplace(overload, i);
      }
    } else if (auto const *gb = t.if_as<type::Generic<type::Block>>()) {
      auto const *i = gb->Instantiate(tv.work_resources(), arguments);
      if (not i) { continue; }
      if (ValidateCallable(i, arguments, errors)) {
        valid.emplace(overload, i);
      }
    } else if (auto const *gs = t.if_as<type::Generic<type::Struct>>()) {
      auto const *i = gs->Instantiate(tv.work_resources(), arguments);
      if (not i) { continue; }
      valid.emplace(overload, type::Type_);
    }
  }
  return valid;
}

}  // namespace

absl::flat_hash_set<module::Module *> ModulesFromTypeProvenance(
    absl::flat_hash_set<type::Type> const &adl_types) {
  absl::flat_hash_set<module::Module *> adl_modules;
  for (type::Type t : adl_types) {
    if (auto const *mod = type::Provenance(t)) {
      // TODO: Remove const_cast.
      adl_modules.insert(const_cast<module::Module *>(mod));
    }
  }
  return adl_modules;
}

std::optional<core::Params<type::QualType>> VerifyParameters(
    TypeVerifier &tv, core::Params<ast::Declaration> const &params) {
  // Parameter types cannot be dependent in concrete implementations so it is
  // safe to verify each of them separately (to generate more errors that are
  // likely correct).

  core::Params<type::QualType> type_params;
  type_params.reserve(params.size());
  bool err = false;
  for (auto &d : params) {
    auto qt = tv.VerifyType(&d.value)[0];
    if (qt.ok()) {
      type_params.append(d.name, qt, d.flags);
    } else {
      err = true;
    }
  }
  if (err) { return std::nullopt; }
  return type_params;
}

std::optional<core::Arguments<type::Typed<ir::CompleteResultRef>>>
VerifyArguments(TypeVerifier &tv,
                absl::Span<ast::Call::Argument const> arguments,
                ir::CompleteResultBuffer &out) {
  bool error = false;
  std::vector<std::pair<type::Type, ssize_t>> refs;
  for (auto const &argument : arguments) {
    auto expr_qual_type = tv.VerifyType(&argument.expr())[0];
    error |= expr_qual_type.HasErrorMark();
    if (error) {
      LOG("VerifyArguments", "Error with: %s", argument.expr().DebugString());
      refs.emplace_back(nullptr, -1);
    } else {
      LOG("VerifyArguments", "constant: %s", argument.expr().DebugString());
      if (expr_qual_type.constant()) {
        if (auto maybe_result = tv.EvaluateToBufferOrDiagnose(
                type::Typed(&argument.expr(), expr_qual_type.type()))) {
          LOG("VerifyArguments", "%s: %s",
              expr_qual_type.type().Representation((*maybe_result)[0]),
              expr_qual_type.type());
          out.append((*maybe_result)[0]);
          refs.emplace_back(expr_qual_type.type(), out.num_entries() - 1);
        }
      } else {
        refs.emplace_back(expr_qual_type.type(), out.num_entries() - 1);
      }
    }
  }

  if (error) { return std::nullopt; }

  core::Arguments<type::Typed<ir::CompleteResultRef>> argument_values;

  size_t i = 0;
  for (auto const &[t, index] : refs) {
    absl::Cleanup c = [&] { ++i; };
    auto ref        = index == -1 ? ir::CompleteResultRef() : out[index];
    if (not arguments[i].named()) {
      argument_values.pos_emplace(ref, t);
    } else {
      argument_values.named_emplace(arguments[i].name(), type::Typed(ref, t));
    }
  }

  return argument_values;
}

std::variant<
    type::Typed<CallMetadata::callee_locator_t>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
VerifyCall(TypeVerifier &tv, VerifyCallParameters const &vcp) {
  auto const &[call, callee, arguments] = vcp;
  LOG("VerifyCall", "%s", call->DebugString());
  absl::flat_hash_map<type::Callable const *, core::CallabilityResult> errors;
  auto exprs =
      ResolveCall(tv, tv.context().CallMetadata(call), arguments, errors);
  if (exprs.size() == 1) {
    auto typed_expr = *exprs.begin();
    tv.context().SetCallMetadata(call, CallMetadata(*typed_expr));
    if (auto *c = callee.get_if<ast::Expression>()) {
      // TODO: This doesn't need to be a constant.
      tv.context().set_qual_type(c,
                                 type::QualType::Constant(typed_expr.type()));
    }
    return typed_expr;
  } else {
    if (auto *c = callee.get_if<ast::Expression>()) {
      tv.context().set_qual_type(c, type::QualType::Error());
    }
    return errors;
  }
}

std::variant<
    std::vector<type::QualType>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
VerifyReturningCall(TypeVerifier &tv, VerifyCallParameters const &vcp) {
  auto result = VerifyCall(tv, vcp);
  if (auto *error = std::get_if<
          absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>(
          &result)) {
    return std::move(*error);
  }

  // TODO: Expansion is relevant too.
  std::vector<type::QualType> qts;
  auto expr = std::get<type::Typed<CallMetadata::callee_locator_t>>(result);
  type::ReturningType const &rt = expr.type().as<type::ReturningType>();
  if (rt.eager()) {
    for (type::Type t : rt.return_types()) {
      qts.push_back(type::QualType::Constant(t));
    }
  } else {
    for (type::Type t : rt.return_types()) {
      qts.push_back(type::QualType::NonConstant(t));
    }
  }

  return qts;
}

}  // namespace compiler
