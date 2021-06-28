#include "compiler/verify/common.h"

#include <optional>
#include <string_view>

#include "compiler/compiler.h"
#include "compiler/module.h"
#include "core/arguments.h"
#include "core/call.h"
#include "ir/value/value.h"
#include "type/callable.h"
#include "type/overload_set.h"
#include "type/provenance.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

type::Typed<ir::Value> EvaluateIfConstant(Compiler &c,
                                          ast::Expression const *expr,
                                          type::QualType qt) {
  if (qt.constant()) {
    LOG("EvaluateIfConstant", "Evaluating constant: %s", expr->DebugString());
    auto maybe_val =
        c.Evaluate(type::Typed<ast::Expression const *>(expr, qt.type()));
    if (maybe_val) { return type::Typed<ir::Value>(*maybe_val, qt.type()); }
    c.diag().Consume(maybe_val.error());
  }
  return type::Typed<ir::Value>(ir::Value(), qt.type());
}

void ExtractParams(
    ast::Expression const *callee, type::Callable const *callable,
    core::Arguments<type::Typed<ir::Value>> const &args,
    std::vector<std::tuple<ast::Expression const *, type::Callable const *,
                           core::Params<type::QualType>>> &overload_params,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>
        &errors) {
  if (auto const *f = callable->if_as<type::Function>()) {
    auto result = core::Callability(f->params(), args,
                                    [](auto const &...) { return true; });
    if (result.ok()) {
      overload_params.emplace_back(callee, f, f->params());
    } else {
      errors.emplace(f, std::move(result));
      return;
    }
  } else if (auto const *os = callable->if_as<type::OverloadSet>()) {
    for (auto const *overload : os->members()) {
      // TODO: Callee provenance is wrong here.
      ExtractParams(callee, overload, args, overload_params, errors);
    }
    if (overload_params.empty()) { return; }
  } else if (auto const *gf = callable->if_as<type::GenericFunction>()) {
    auto result = core::Callability(gf->params(), args,
                                    [](auto const &...) { return true; });

    if (result.ok()) {
      // TODO: But this could fail and when it fails we want to capture failure
      // reasons.
      auto const *f = gf->concrete(args);
      overload_params.emplace_back(callee, f, f->params());
    } else {
      errors.emplace(gf, std::move(result));
      return;
    }
  } else if (auto const *gs = callable->if_as<type::GenericStruct>()) {
    // TODO: But this could fail and when it fails we want to capture failure
    // reasons.
    auto [params, s] = gs->Instantiate(args);
    overload_params.emplace_back(callee, gs, params);
  } else {
    UNREACHABLE();
  }
}

template <typename IndexT>
void AddType(IndexT &&index, type::Type t,
             std::vector<core::Arguments<type::Type>> *args) {
  std::for_each(
      args->begin(), args->end(), [&](core::Arguments<type::Type> &fnargs) {
        if constexpr (base::meta<std::decay_t<IndexT>> == base::meta<size_t>) {
          fnargs.pos_emplace(t);
        } else {
          fnargs.named_emplace(index, t);
        }
      });
}

// TODO: Ideally we wouldn't create these all at once but rather iterate through
// the possibilities. Doing this the right way involves having sum and product
// iterators.
std::vector<core::Arguments<type::Type>> ExpandedArguments(
    core::Arguments<type::QualType> const &arguments) {
  std::vector<core::Arguments<type::Type>> all_expanded_options(1);
  arguments.ApplyWithIndex([&](auto &&index, type::QualType r) {
    // TODO: also maybe need the expression this came from to see if it needs
    // to be expanded.
    AddType(index, r.type(), &all_expanded_options);
  });

  return all_expanded_options;
}

void AddOverloads(Context const &context, ast::Expression const *callee,
                  absl::flat_hash_map<ast::Expression const *,
                                      type::Callable const *> &overload_map) {
  auto const *overloads = context.AllOverloads(callee);
  if (not overloads) { return; }
  for (auto const *overload : overloads->members()) {
    LOG("AddOverloads", "Callee: %p %s", overload, overload->DebugString());
    type::QualType qt = RetrieveQualTypes(context, overload)[0];

    if (qt) { overload_map.emplace(overload, &qt.type().as<type::Callable>()); }
  }
}

}  // namespace

// TODO: There's something strange about this: We want to work on a temporary
// data/compiler, but using `this` makes it feel more permanent.
core::Params<std::pair<ir::Value, type::QualType>>
Compiler::ComputeParamsFromArgs(
    ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  LOG("generic-fn", "Creating a concrete implementation with %s",
      args.Transform([](auto const &a) { return a.type().to_string(); }));

  core::Params<std::pair<ir::Value, type::QualType>> parameters(
      node->params().size());

  for (auto [index, dep_node] : node->ordered_dependency_nodes()) {
    ASSERT(dep_node.node()->ids().size() == 1u);
    std::string_view id = dep_node.node()->ids()[0].name();
    LOG("generic-fn", "Handling dep-node %s`%s`", ToString(dep_node.kind()),
        id);
    switch (dep_node.kind()) {
      case core::DependencyNodeKind::ArgValue: {
        ir::Value val;
        if (index < args.pos().size()) {
          val = *args[index];
        } else if (auto const *a = args.at_or_null(id)) {
          val = **a;
        } else {
          auto const *init_val = ASSERT_NOT_NULL(dep_node.node()->init_val());
          type::Type t         = context().arg_type(id);
          auto maybe_val =
              Evaluate(type::Typed<ast::Expression const *>(init_val, t));
          if (not maybe_val) { NOT_YET(); }
          val = *maybe_val;
        }

        // Erase values not known at compile-time.
        if (val.get_if<ir::Reg>()) { val = ir::Value(); }

        LOG("generic-fn", "... %s", val);
        context().set_arg_value(id, val);
      } break;
      case core::DependencyNodeKind::ArgType: {
        type::Type arg_type = nullptr;
        if (index < args.pos().size()) {
          arg_type = args[index].type();
        } else if (auto const *a = args.at_or_null(id)) {
          arg_type = a->type();
        } else {
          // TODO: What if this is a bug and you don't have an initial value?
          auto *init_val = ASSERT_NOT_NULL(dep_node.node()->init_val());
          arg_type       = VerifyType(init_val)[0].type();
        }
        LOG("generic-fn", "... %s", arg_type.to_string());
        context().set_arg_type(id, arg_type);
      } break;
      case core::DependencyNodeKind::ParamType: {
        type::Type t;
        if (auto const *type_expr = dep_node.node()->type_expr()) {
          auto type_expr_type = VerifyType(type_expr)[0].type();
          if (type_expr_type != type::Type_) {
            NOT_YET("log an error: ", type_expr->DebugString(), ": ",
                    type_expr_type);
          }

          ASSIGN_OR(NOT_YET(),  //
                    t, EvaluateOrDiagnoseAs<type::Type>(type_expr));
        } else {
          t = VerifyType(dep_node.node()->init_val())[0].type();
        }

        auto qt = (dep_node.node()->flags() & ast::Declaration::f_IsConst)
                      ? type::QualType::Constant(t)
                      : type::QualType::NonConstant(t);

        // TODO: Once a parameter type has been computed, we know it's
        // argument type has already been computed so we can verify that the
        // implicit casts are allowed.
        LOG("generic-fn", "... %s", qt.to_string());
        size_t i = *ASSERT_NOT_NULL(node->params().at_or_null(id));
        parameters.set(i, core::Param<std::pair<ir::Value, type::QualType>>(
                              id, std::make_pair(ir::Value(), qt),
                              node->params()[i].flags));
      } break;
      case core::DependencyNodeKind::ParamValue: {
        // Find the argument associated with this parameter.
        // TODO, if the type is wrong but there is an implicit cast, deal with
        // that.
        type::Typed<ir::Value> arg;
        if (index < args.pos().size()) {
          arg = args[index];
          LOG("generic-fn", "%s %s", *arg, arg.type().to_string());
        } else if (auto const *a = args.at_or_null(id)) {
          arg = *a;
        } else {
          auto t         = context().qual_types(dep_node.node())[0].type();
          auto maybe_val = Evaluate(type::Typed<ast::Expression const *>(
              ASSERT_NOT_NULL(dep_node.node()->init_val()), t));
          if (not maybe_val) { diag().Consume(maybe_val.error()); }
          arg = type::Typed<ir::Value>(*maybe_val, t);
          LOG("generic-fn", "%s", dep_node.node()->DebugString());
        }

        // TODO: Support multiple declarations
        if (not context().Constant(&dep_node.node()->ids()[0])) {
          // TODO complete?
          // TODO: Support multiple declarations
          context().SetConstant(&dep_node.node()->ids()[0], *arg);
        }

        size_t i = *ASSERT_NOT_NULL(node->params().at_or_null(id));
        parameters[i].value.first = *arg;
      } break;
    }
  }
  return parameters;
}

std::optional<core::Params<type::QualType>> Compiler::VerifyParams(
    core::Params<std::unique_ptr<ast::Declaration>> const &params) {
  // Parameter types cannot be dependent in concrete implementations so it is
  // safe to verify each of them separately (to generate more errors that are
  // likely correct).

  core::Params<type::QualType> type_params;
  type_params.reserve(params.size());
  bool err = false;
  for (auto &d : params) {
    auto qt = VerifyType(d.value.get())[0];
    if (qt.ok()) {
      type_params.append(d.name, qt, d.flags);
    } else {
      err = true;
    }
  }
  if (err) { return std::nullopt; }
  return type_params;
}

std::optional<core::Arguments<type::Typed<ir::Value>>>
Compiler::VerifyArguments(absl::Span<ast::Call::Argument const> args) {
  bool err = false;
  core::Arguments<type::Typed<ir::Value>> arg_vals;
  for (auto const &arg : args) {
    type::Typed<ir::Value> result;
    auto expr_qual_type = VerifyType(&arg.expr())[0];
    err |= not expr_qual_type.ok();
    if (err) {
      LOG("VerifyArguments", "Error with: %s", arg.expr().DebugString());
      result = type::Typed<ir::Value>(ir::Value(), nullptr);
    } else {
      LOG("VerifyArguments", "constant: %s", arg.expr().DebugString());
      result = EvaluateIfConstant(*this, &arg.expr(), expr_qual_type);
    }
    if (not arg.named()) {
      arg_vals.pos_emplace(result);
    } else {
      arg_vals.named_emplace(arg.name(), result);
    }
  }

  if (err) { return std::nullopt; }
  return arg_vals;
}

std::optional<core::Arguments<type::Typed<ir::Value>>>
Compiler::VerifyArguments(
    core::Arguments<ast::Expression const *> const &args) {
  bool err      = false;
  auto arg_vals = args.Transform([&](ast::Expression const *expr) {
    auto expr_qual_type = VerifyType(expr)[0];
    err |= not expr_qual_type.ok();
    if (err) {
      LOG("VerifyArguments", "Error with: %s", expr->DebugString());
      return type::Typed<ir::Value>(ir::Value(), nullptr);
    }
    LOG("VerifyArguments", "constant: %s", expr->DebugString());
    return EvaluateIfConstant(*this, expr, expr_qual_type);
  });

  if (err) { return std::nullopt; }
  return arg_vals;
}

// TODO: Replace `symbol` with an enum.
type::QualType Compiler::VerifyUnaryOverload(
    char const *symbol, ast::Expression const *node,
    type::Typed<ir::Value> const &operand) {
  absl::flat_hash_set<type::Callable const *> member_types;

  node->scope()->ForEachDeclIdTowardsRoot(
      symbol, [&](ast::Declaration::Id const *id) {
        ASSIGN_OR(return false, auto qt, context().qual_types(id)[0]);
        // Must be callable because we're looking at overloads for operators
        // which have previously been type-checked to ensure callability.
        auto &c = qt.type().as<type::Callable>();
        member_types.insert(&c);
        return true;
      });

  if (member_types.empty()) {
    return context().set_qual_type(node, type::QualType::Error())[0];
  }
  std::vector<type::Typed<ir::Value>> pos_args;
  pos_args.emplace_back(operand);

  // TODO: Check that we only have one return type on each of these overloads.
  return type::QualType(
      type::MakeOverloadSet(std::move(member_types))
          ->return_types(core::Arguments<type::Typed<ir::Value>>(
              std::move(pos_args), {}))[0],
      type::Quals::Unqualified());
}

// TODO: Replace `symbol` with an enum.
type::QualType Compiler::VerifyBinaryOverload(
    std::string_view symbol, ast::Expression const *node,
    type::Typed<ir::Value> const &lhs, type::Typed<ir::Value> const &rhs) {
  absl::flat_hash_set<type::Callable const *> member_types;

  node->scope()->ForEachDeclIdTowardsRoot(
      symbol, [&](ast::Declaration::Id const *id) {
        ASSIGN_OR(return false, auto qt, context().qual_types(id)[0]);
        // Must be callable because we're looking at overloads for operators
        // which have previously been type-checked to ensure callability.
        auto &c = qt.type().as<type::Callable>();
        member_types.insert(&c);
        return true;
      });

  if (member_types.empty()) { return type::QualType::Error(); }
  std::vector<type::Typed<ir::Value>> pos_args;
  pos_args.emplace_back(lhs);
  pos_args.emplace_back(rhs);
  // TODO: Check that we only have one return type on each of these overloads.
  return type::QualType(
      type::MakeOverloadSet(std::move(member_types))
          ->return_types(core::Arguments<type::Typed<ir::Value>>(
              std::move(pos_args), {}))[0],
      type::Quals::Unqualified());
}

std::pair<type::QualType,
          absl::flat_hash_map<ast::Expression const *, type::Callable const *>>
Compiler::VerifyCallee(
    ast::Expression const *callee,
    core::Arguments<type::Typed<ir::Value>> const &args,
    absl::flat_hash_set<type::Type> const &argument_dependent_lookup_types) {
  using return_type =
      std::pair<type::QualType, absl::flat_hash_map<ast::Expression const *,
                                                    type::Callable const *>>;

  absl::flat_hash_map<ast::Expression const *, type::Callable const *>
      overload_map;

  LOG("VerifyCallee", "Verify callee: %s", callee->DebugString());

  // Set modules to be used for ADL before calling VerifyType on the callee, so
  // the verifier knows which contexts to look things up in.
  if (auto const *id = callee->if_as<ast::Identifier>()) {
    absl::flat_hash_set<compiler::CompiledModule const *> adl_modules;
    for (type::Type t : argument_dependent_lookup_types) {
      if (auto const *mod = type::Provenance(t)) {
        if (mod == &context().module()) { continue; }
        adl_modules.insert(&mod->as<compiler::CompiledModule>());
      }
    }

    context().SetAdlModules(id, std::move(adl_modules));
  }

  ASSIGN_OR(return return_type(type::QualType::Error(), {}),  //
                   auto qt, VerifyType(callee)[0]);

  ASSIGN_OR(return return_type(qt, {}),  //
                   auto const &callable, qt.type().if_as<type::Callable>());

  AddOverloads(context(), callee, overload_map);
  for (type::Type t : argument_dependent_lookup_types) {
    // TODO: Generic structs? Arrays? Pointers?
    if (auto const *s = t.if_as<type::Struct>()) {
      AddOverloads(s->defining_module()->as<compiler::CompiledModule>().context(
                       &context().module()),
                   callee, overload_map);
    }
  }

  return return_type(qt, std::move(overload_map));
}

std::variant<
    std::vector<type::QualType>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
Compiler::VerifyCall(
    ast::Call const *call_expr,
    absl::flat_hash_map<ast::Expression const *, type::Callable const *> const
        &overload_map,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  LOG("VerifyCall", "%s", call_expr->DebugString());
  absl::flat_hash_map<type::Callable const *, core::CallabilityResult> errors;
  std::vector<std::tuple<ast::Expression const *, type::Callable const *,
                         core::Params<type::QualType>>>
      overload_params;

  // TODO: Is it possible that the returned references in `AllOverloads` is
  // invalidated during some computation of `ExtractParams`? Maybe if something
  // else is inserted into the map. I believe not even if something is inserted
  // the iterator into members is still valid because there's an extra layer of
  // indirection in the overload set. Do we really want to rely on this?!
  if (auto const *overloads = context().AllOverloads(call_expr->callee())) {
    for (auto const *callee : overloads->members()) {
      type::QualType qt = RetrieveQualTypes(context(), callee)[0];
      ExtractParams(callee, &qt.type().as<type::Callable>(), args,
                    overload_params, errors);
    }
  }

  LOG("VerifyCall", "%u overloads", overload_params.size());

  // TODO: Expansion is relevant too.
  std::vector<std::vector<type::Type>> return_types;

  // TODO: Take a type::Typed<ir::Value> instead.
  type::Quals quals = type::Quals::Const();
  auto args_qt      = args.Transform([&](auto const &typed_value) {
    auto qt = typed_value->empty()
                  ? type::QualType::NonConstant(typed_value.type())
                  : type::QualType::Constant(typed_value.type());
    quals &= qt.quals();
    return qt;
  });

  ast::OverloadSet os;
  for (auto const &expansion : ExpandedArguments(args_qt)) {
    for (auto const &[callee, callable_type, params] : overload_params) {
      LOG("VerifyCall", "Callable type of overload: %s",
          callable_type->to_string());
      // TODO: Assuming this is unambiguously callable is a bit of a stretch.

      // TODO: `core::IsCallable` already does this but doesn't give us access
      // to writing errors. Rewriting it here and then we'll look at how to
      // combine it later.

      ASSERT(expansion.pos().size() <= params.size());
      for (size_t i = 0; i < expansion.pos().size(); ++i) {
        LOG("VerifyCall", "Comparing parameter %s with argument %s",
            expansion[i].to_string(), params[i].value.type().to_string());
        if (not type::CanCastImplicitly(expansion[i], params[i].value.type())) {
          // TODO: Currently as soon as we find an error with a call we move on.
          // It'd be nice to extract all the error information for each.
          errors.emplace(callable_type, core::CallabilityResult::TypeMismatch{
                                            .parameter = i,
                                            .argument  = expansion[i],
                                        });
          goto next_overload;
        }
      }

      // Note: Missing/defaultable has already been handled.
      for (size_t i = expansion.pos().size(); i < params.size(); ++i) {
        auto const &param = params[i];
        auto const *arg   = expansion.at_or_null(param.name);
        // It's okay if this argument is missing. We've already checked that all
        // required arguments (non-defaultable) are present, so this argument
        // missing means this must be defaultable.
        if (not arg) { continue; }

        if (not type::CanCastImplicitly(*arg, param.value.type())) {
          // TODO: Currently as soon as we find an error with a call we move on.
          // It'd be nice to extract all the error information for each.
          errors.emplace(callable_type, core::CallabilityResult::TypeMismatch{
                                            .parameter = param.name,
                                            .argument  = expansion[param.name],
                                        });
          goto next_overload;
        }
      }

      os.insert(callee);
      LOG("VerifyCall", "Inserting, %s", callable_type->return_types(args));
      if (not callable_type->is<type::GenericStruct>()) {
        quals &= ~type::Quals::Const();
      }
      return_types.push_back(callable_type->return_types(args));
      goto next_expansion;
    next_overload:;
    }

    return errors;
  next_expansion:;
  }

  context().SetViableOverloads(call_expr->callee(), std::move(os));

  ASSERT(return_types.size() == 1u);
  std::vector<type::QualType> qts;
  qts.reserve(return_types.front().size());
  for (type::Type t : return_types.front()) { qts.emplace_back(t, quals); }
  return qts;
}

std::vector<core::Arguments<type::QualType>> YieldArgumentTypes(
    Context const &context,
    base::PtrUnion<ast::BlockNode const, ast::ScopeNode const> node) {
  std::vector<core::Arguments<type::QualType>> yield_types;
  absl::Span<ast::YieldStmt const *const> yields = context.YieldsTo(node);
  yield_types.reserve(yields.size());

  for (auto const *yield_stmt : yields) {
    auto &yielded = yield_types.emplace_back();
    for (const auto *expr : yield_stmt->exprs()) {
      // TODO: Determine whether or not you want to support named yields. If
      // not, reduce this to a vector or some other positional arguments type.
      yielded.pos_emplace(context.qual_types(expr)[0]);
    }
  }
  return yield_types;
}

absl::Span<type::QualType const> RetrieveQualTypes(
    Context const &c, ast::Expression const *expr) {
  auto const &expr_mod = expr->scope()
                             ->Containing<ast::ModuleScope>()
                             ->module()
                             ->as<CompiledModule>();
  auto &mod = c.module();
  return (&mod == &expr_mod) ? c.qual_types(expr)
                             : expr_mod.context(&mod).qual_types(expr);
}

}  // namespace compiler
