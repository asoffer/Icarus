#include "compiler/verify/common.h"

#include <optional>
#include <string_view>

#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/compiler.h"
#include "compiler/module.h"
#include "core/arguments.h"
#include "core/call.h"
#include "type/callable.h"
#include "type/cast.h"
#include "type/overload_set.h"
#include "type/provenance.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

void ValidateCallable(
    ast::Expression const *e, type::Callable const *callable,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arguments,
    absl::flat_hash_set<type::Typed<ast::Expression const *>> &valid,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>
        &errors) {
  auto callability = core::Callability(
      callable->params(), arguments,
      [](type::Typed<ir::CompleteResultRef> const &argument,
         type::QualType const &parameter) {
        return type::CanCastImplicitly(argument.type(), parameter.type());
      });
  if (callability.ok()) {
    valid.emplace(e, type::Type(callable));
  } else {
    errors.emplace(callable, std::move(callability));
  }
}

absl::flat_hash_set<type::Typed<ast::Expression const *>> ResolveCall(
    Compiler &c, CallMetadata const &metadata,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arguments,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>
        &errors) {
  // TODO: if (auto const *r = metadata.resolved()) { return {r}; }

  absl::flat_hash_set<type::Typed<ast::Expression const *>> valid;

  Context const &context_root = c.context().root();

  for (auto const *overload : metadata.overloads()) {
    Context const &callee_root =
        ModuleFor(overload)->as<CompiledModule>().context();
    // TODO: This is fraught, because we still don't have access to
    // instantiated contexts if that's what's needed here.
    type::Type t;
    if (&context_root == &callee_root) {
      t = c.VerifyType(overload)[0].type();
    } else {
      t = callee_root.qual_types(overload)[0].type();
    }

    if (auto const *callable = t.if_as<type::Callable>()) {
      ValidateCallable(overload, callable, arguments, valid, errors);
    } else if (auto const *gf = t.if_as<type::Generic<type::Function>>()) {
      auto const *i = gf->Instantiate(c.work_resources(), arguments);
      if (not i) { continue; }  // TODO: Save an error.
      ValidateCallable(overload, i, arguments, valid, errors);
    } else if (auto const *gb = t.if_as<type::Generic<type::Block>>()) {
      auto const *i = gb->Instantiate(c.work_resources(), arguments);
      if (not i) { continue; }
      ValidateCallable(overload, i, arguments, valid, errors);
    }
  }
  return valid;
}

}  // namespace

absl::flat_hash_set<module::BasicModule const *> ModulesFromTypeProvenance(
    absl::flat_hash_set<type::Type> const &adl_types) {
  absl::flat_hash_set<module::BasicModule const *> adl_modules;
  for (type::Type t : adl_types) {
    if (auto const *mod = type::Provenance(t)) { adl_modules.insert(mod); }
  }
  return adl_modules;
}

std::optional<core::Params<type::QualType>> VerifyParameters(
    Compiler &c,
    core::Params<std::unique_ptr<ast::Declaration>> const &params) {
  // Parameter types cannot be dependent in concrete implementations so it is
  // safe to verify each of them separately (to generate more errors that are
  // likely correct).

  core::Params<type::QualType> type_params;
  type_params.reserve(params.size());
  bool err = false;
  for (auto &d : params) {
    auto qt = c.VerifyType(d.value.get())[0];
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
VerifyArguments(Compiler &c, absl::Span<ast::Call::Argument const> arguments,
                ir::CompleteResultBuffer &out) {
  bool error = false;
  std::vector<std::pair<type::Type, ssize_t>> refs;
  for (auto const &argument : arguments) {
    auto expr_qual_type = c.VerifyType(&argument.expr())[0];
    error |= expr_qual_type.HasErrorMark();
    if (error) {
      LOG("VerifyArguments", "Error with: %s", argument.expr().DebugString());
      refs.emplace_back(nullptr, -1);
    } else {
      LOG("VerifyArguments", "constant: %s", argument.expr().DebugString());
      if (expr_qual_type.constant()) {
        if (auto maybe_result = c.EvaluateToBufferOrDiagnose(
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
    type::Typed<ast::Expression const *>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
VerifyCall(Compiler &c, VerifyCallParameters const &vcp) {
  auto const &[call, callee, arguments] = vcp;
  LOG("VerifyCall", "%s", call->DebugString());
  absl::flat_hash_map<type::Callable const *, core::CallabilityResult> errors;
  auto exprs =
      ResolveCall(c, c.context().CallMetadata(call), arguments, errors);
  if (exprs.size() == 1) {
    auto typed_expr = *exprs.begin();
    c.context().SetCallMetadata(call, CallMetadata(*typed_expr));
    // TODO: This doesn't need to be a constant.
    c.context().set_qual_type(callee,
                              type::QualType::Constant(typed_expr.type()));
    return typed_expr;
  } else {
    c.context().set_qual_type(callee, type::QualType::Error());
    return errors;
  }
}

std::variant<
    std::vector<type::QualType>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
VerifyReturningCall(Compiler &c, VerifyCallParameters const &vcp) {
  auto result = VerifyCall(c, vcp);
  if (auto *error = std::get_if<
          absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>(
          &result)) {
    return std::move(*error);
  }

  // TODO: Expansion is relevant too.
  std::vector<type::QualType> qts;
  auto expr = std::get<type::Typed<ast::Expression const *>>(result);
  for (type::Type t : expr.type().as<type::ReturningType>().return_types()) {
    qts.push_back(type::QualType::NonConstant(t));
  }

  return qts;
}

}  // namespace compiler
