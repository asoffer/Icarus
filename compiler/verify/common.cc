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
// TODO: Remove need for forward declaration.
struct CompiledModule;

namespace {

void ExtractParameters(
    Compiler &compiler, ast::Expression const *callee, type::Type t,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &arguments,
    std::vector<std::tuple<ast::Expression const *, type::Callable const *,
                           core::Params<type::QualType>>> &overload_params,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>
        &errors) {
  if (auto const *f = t.if_as<type::Callable>()) {
    auto result = core::Callability(f->params(), arguments,
                                    [](auto const &...) { return true; });
    if (result.ok()) {
      overload_params.emplace_back(callee, f, f->params());
    } else {
      errors.emplace(f, std::move(result));
      return;
    }
  } else if (auto const *os = t.if_as<type::OverloadSet>()) {
    for (type::Type overload : os->members()) {
      // TODO: Callee provenance is wrong here.
      ExtractParameters(compiler, callee, overload, arguments, overload_params,
                        errors);
    }
    if (overload_params.empty()) { return; }
  } else if (auto const *gf = t.if_as<type::Generic<type::Function>>()) {
    auto const *i = gf->Instantiate(compiler.work_resources(), arguments);
    if (not i) { return; }
    overload_params.emplace_back(callee, i, i->params());
  } else if (auto const *gb = t.if_as<type::Generic<type::Block>>()) {
    auto const *i = gb->Instantiate(compiler.work_resources(), arguments);
    if (not i) { return; }
    overload_params.emplace_back(callee, i, i->params());
  } else {
    UNREACHABLE();
  }
}

}  // namespace

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
    error |= not expr_qual_type.ok();
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

absl::flat_hash_set<CompiledModule const *> ModulesFromTypeProvenance(
    absl::flat_hash_set<type::Type> const &adl_types) {
  absl::flat_hash_set<CompiledModule const *> adl_modules;
  for (type::Type t : adl_types) {
    if (auto const *mod = type::Provenance(t)) {
      adl_modules.insert(&mod->as<compiler::CompiledModule>());
    }
  }
  return adl_modules;
}

type::QualType VerifyCallee(Compiler &c, ast::Expression const *callee,
                            absl::flat_hash_set<type::Type> const &adl_types) {
  LOG("VerifyCallee", "Verify callee: %s", callee->DebugString());

  // Set modules to be used for ADL before calling VerifyType on the callee, so
  // the verifier knows which contexts to look things up in.
  if (auto const *id = callee->if_as<ast::Identifier>()) {
    auto adl_modules = ModulesFromTypeProvenance(adl_types);
    adl_modules.erase(c.resources().module);
    c.context().SetAdlModules(id, std::move(adl_modules));
  }

  return c.VerifyType(callee)[0];
}

std::variant<ast::OverloadSet, absl::flat_hash_map<type::Callable const *,
                                                   core::CallabilityResult>>
VerifyCall(Compiler &c, VerifyCallParameters const &vcp) {
  auto const & [callee, arguments] = vcp;
  LOG("VerifyCall", "%s", callee->DebugString());

  absl::flat_hash_map<type::Callable const *, core::CallabilityResult> errors;
  std::vector<std::tuple<ast::Expression const *, type::Callable const *,
                         core::Params<type::QualType>>>
      overload_params;

  if (auto const *overloads = c.context().AllOverloads(callee)) {
    Context const &context_root = c.context().root();
    for (auto const *overload : overloads->members()) {
      Context const &callee_root =
          ModuleFor(overload)->as<CompiledModule>().context();
      // TODO: This is fraught, because we still don't have access to
      // instantiated contexts if that's what's needed here.
      auto &ctx = (&context_root == &callee_root) ? c.context() : callee_root;
      type::QualType qt = ctx.qual_types(overload)[0];
      ExtractParameters(c, overload, qt.type(), arguments, overload_params, errors);
    }
  }

  LOG("VerifyCall", "%u overloads", overload_params.size());
  type::Quals quals = type::Quals::Const();
  auto args_qt      = arguments.Transform([&](auto const &typed_value) {
    auto qt = typed_value->empty()
                  ? type::QualType::NonConstant(typed_value.type())
                  : type::QualType::Constant(typed_value.type());
    quals &= qt.quals();
    return qt;
  });

  ast::OverloadSet os;
  for (auto const &[called, callable_type, params] : overload_params) {
    LOG("VerifyCall", "Callable type of overload: %s",
        callable_type->to_string());

    auto callability = core::Callability(
        params, args_qt,
        [](type::QualType const &argument, type::QualType const &parameter) {
          return type::CanCastImplicitly(argument.type(), parameter.type());
        });
    if (callability.ok()) {
      os.insert(called);
      if (not callable_type->is<type::Generic<type::Struct>>()) {
        quals &= ~type::Quals::Const();
      }
    } else {
      errors.emplace(callable_type, std::move(callability));
    }
  }

  if (os.members().empty()) { return errors; }
  c.context().SetViableOverloads(callee, os);
  return os;
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
  std::vector<std::vector<type::Type>> return_types;

  auto const &[callee, arguments] = vcp;
  auto const &os                  = std::get<ast::OverloadSet>(result);

  for (auto const *member : os.members()) {
    Context const &callee_root =
        ModuleFor(member)->as<CompiledModule>().context();
    // TODO: This is fraught, because we still don't have access to
    // instantiated contexts if that's what's needed here.
    Context const &context_root = c.context().root();
    auto &ctx = (&context_root == &callee_root) ? c.context() : callee_root;

    type::Type t = ctx.qual_types(member)[0].type();
    if (type::ReturningType const *rt = t.if_as<type::ReturningType>()) {
      return_types.emplace_back(rt->return_types().begin(),
                                rt->return_types().end());
    } else if (auto const *gf = t.if_as<type::Generic<type::Function>>()) {
      auto const *i = gf->Instantiate(c.work_resources(), arguments);
      if (not i) { continue; }
      return_types.emplace_back(i->return_types().begin(),
                                i->return_types().end());
    } else if (auto const *gb = t.if_as<type::Generic<type::Block>>()) {
      return_types.emplace_back();
    }
  }

  ASSERT(return_types.size() == 1u);
  std::vector<type::QualType> qts;
  qts.reserve(return_types.front().size());
  for (type::Type t : return_types.front()) {
    qts.push_back(type::QualType::NonConstant(t));
  }
  return qts;
}

std::vector<core::Arguments<type::QualType>> YieldArgumentTypes(
    Context const &context,
    base::PtrUnion<ast::BlockNode const, ast::ScopeNode const,
                   ast::IfStmt const, ast::WhileStmt const>
        node) {
  std::vector<core::Arguments<type::QualType>> yield_types;
  absl::Span<ast::YieldStmt const *const> yields = context.YieldsTo(node);
  yield_types.reserve(yields.size());

  for (auto const *yield_stmt : yields) {
    auto &yielded = yield_types.emplace_back();
    for (auto const &argument : yield_stmt->arguments()) {
      // TODO: Determine whether or not you want to support named yields. If
      // not, reduce this to a vector or some other positional arguments type.
      yielded.pos_emplace(context.qual_types(&argument.expr())[0]);
    }
  }
  return yield_types;
}

}  // namespace compiler
