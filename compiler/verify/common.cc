#include "compiler/verify/common.h"

#include <optional>
#include <string_view>

#include "compiler/compiler.h"
#include "compiler/library_module.h"
#include "core/call.h"
#include "core/arguments.h"
#include "ir/value/value.h"
#include "type/callable.h"
#include "type/overload_set.h"
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

// Determines which arguments are passed to which parameters. No type-checking
// is done in this phase. Matching arguments to parameters can be done, even on
// generics without any type-checking.
template <typename Ignored>
std::optional<Compiler::CallError::ErrorReason> MatchArgumentsToParameters(
    core::Params<Ignored> const &params,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  if (args.size() > params.size()) {
    return Compiler::CallError::TooManyArguments{
        .num_provided     = args.size(),
        .max_num_accepted = params.size(),
    };
  }

  absl::flat_hash_set<std::string> missing_non_defaultable;

  for (size_t i = args.pos().size(); i < params.size(); ++i) {
    auto const &param = params[i];
    LOG("match", "Matching param in position %u (name = %s)", i, param.name);
    if (args.at_or_null(param.name)) { continue; }

    // No argument provided by that name? This could be because we have
    // default parameters or an empty variadic pack.
    // TODO: Handle variadic packs.
    if (not(param.flags & core::HAS_DEFAULT)) {
      missing_non_defaultable.insert(param.name);
    }
  }

  // TODO: Instead of early exit get all relevant errors.
  if (not missing_non_defaultable.empty()) {
    return Compiler::CallError::MissingNonDefaultableArguments{
        .names = std::move(missing_non_defaultable),
    };
  }

  for (auto const &[name, val] : args.named()) {
    auto const *index = params.at_or_null(name);
    if (not index) {
      return Compiler::CallError::NoParameterNamed{.name = name};
    } else if (*index < args.pos().size()) {
      return Compiler::CallError::PositionalArgumentNamed{.index = *index,
                                                          .name  = name};
    }
  }

  return std::nullopt;
}

void ExtractParams(
    ast::Expression const *callee, type::Callable const *callable,
    core::Arguments<type::Typed<ir::Value>> const &args,
    std::vector<std::tuple<ast::Expression const *, type::Callable const *,
                           core::Params<type::QualType>>> &overload_params,
    Compiler::CallError &errors) {
  Compiler::CallError error;
  if (auto const *f = callable->if_as<type::Function>()) {
    if (auto error_reason = MatchArgumentsToParameters(f->params(), args)) {
      errors.reasons.emplace(f, *std::move(error_reason));
      return;
    } else {
      overload_params.emplace_back(callee, f, f->params());
    }
  } else if (auto const *os = callable->if_as<type::OverloadSet>()) {
    for (auto const *overload : os->members()) {
      // TODO: Callee provenance is wrong here.
      ExtractParams(callee, overload, args, overload_params, errors);
    }
    if (overload_params.empty()) { return; }
  } else if (auto const *gf = callable->if_as<type::GenericFunction>()) {
    if (auto error_reason = MatchArgumentsToParameters(gf->params(), args)) {
      errors.reasons.emplace(gf, *std::move(error_reason));
      return;
    } else {
      // TODO: But this could fail and when it fails we want to capture failure
      // reasons.
      auto const *f = gf->concrete(args);
      overload_params.emplace_back(callee, f, f->params());
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

}  // namespace

// TODO: There's something strange about this: We want to work on a temporary
// data/compiler, but using `this` makes it feel more permanent.
core::Params<std::pair<ir::Value, type::QualType>>
Compiler::ComputeParamsFromArgs(
    ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  LOG("generic-fn", "Creating a concrete implementation with %s",
      args.Transform([](auto const &a) { return a.type()->to_string(); }));

  core::Params<std::pair<ir::Value, type::QualType>> parameters(
      node->params().size());

  for (auto [index, dep_node] : node->ordered_dependency_nodes()) {
    LOG("generic-fn", "Handling dep-node %s`%s`", ToString(dep_node.kind()),
        dep_node.node()->id());
    switch (dep_node.kind()) {
      case core::DependencyNodeKind::ArgValue: {
        ir::Value val;
        if (index < args.pos().size()) {
          val = *args[index];
        } else if (auto const *a = args.at_or_null(dep_node.node()->id())) {
          val = **a;
        } else {
          auto const *init_val = ASSERT_NOT_NULL(dep_node.node()->init_val());
          type::Type t         = context().arg_type(dep_node.node()->id());
          auto maybe_val =
              Evaluate(type::Typed<ast::Expression const *>(init_val, t));
          if (not maybe_val) { NOT_YET(); }
          val = *maybe_val;
        }

        // Erase values not known at compile-time.
        if (val.get_if<ir::Reg>()) { val = ir::Value(); }

        LOG("generic-fn", "... %s", val);
        context().set_arg_value(dep_node.node()->id(), val);
      } break;
      case core::DependencyNodeKind::ArgType: {
        type::Type arg_type = nullptr;
        if (index < args.pos().size()) {
          arg_type = args[index].type();
        } else if (auto const *a = args.at_or_null(dep_node.node()->id())) {
          arg_type = a->type();
        } else {
          // TODO: What if this is a bug and you don't have an initial value?
          auto *init_val = ASSERT_NOT_NULL(dep_node.node()->init_val());
          arg_type       = VerifyType(init_val).type();
        }
        LOG("generic-fn", "... %s", arg_type->to_string());
        context().set_arg_type(dep_node.node()->id(), arg_type);
      } break;
      case core::DependencyNodeKind::ParamType: {
        type::Type t = nullptr;
        if (auto const *type_expr = dep_node.node()->type_expr()) {
          auto type_expr_type = VerifyType(type_expr).type();
          if (type_expr_type != type::Type_) {
            NOT_YET("log an error: ", type_expr->DebugString(), ": ",
                    type_expr_type);
          }

          ASSIGN_OR(NOT_YET(),  //
                    t, EvaluateOrDiagnoseAs<type::Type>(type_expr));
        } else {
          t = VerifyType(dep_node.node()->init_val()).type();
        }

        auto qt = (dep_node.node()->flags() & ast::Declaration::f_IsConst)
                      ? type::QualType::Constant(t)
                      : type::QualType::NonConstant(t);

        // TODO: Once a parameter type has been computed, we know it's
        // argument type has already been computed so we can verify that the
        // implicit casts are allowed.
        LOG("generic-fn", "... %s", qt.to_string());
        size_t i =
            *ASSERT_NOT_NULL(node->params().at_or_null(dep_node.node()->id()));
        parameters.set(
            i, core::Param<std::pair<ir::Value, type::QualType>>(
                   dep_node.node()->id(), std::make_pair(ir::Value(), qt),
                   node->params()[i].flags));
      } break;
      case core::DependencyNodeKind::ParamValue: {
        // Find the argument associated with this parameter.
        // TODO, if the type is wrong but there is an implicit cast, deal with
        // that.
        type::Typed<ir::Value> arg;
        if (index < args.pos().size()) {
          arg = args[index];
          LOG("generic-fn", "%s %s", *arg, arg.type()->to_string());
        } else if (auto const *a = args.at_or_null(dep_node.node()->id())) {
          arg = *a;
        } else {
          auto t         = type_of(dep_node.node());
          auto maybe_val = Evaluate(type::Typed<ast::Expression const *>(
              ASSERT_NOT_NULL(dep_node.node()->init_val()), t));
          if (not maybe_val) { diag().Consume(maybe_val.error()); }
          arg = type::Typed<ir::Value>(*maybe_val, t);
          LOG("generic-fn", "%s", dep_node.node()->DebugString());
        }

        if (not context().Constant(dep_node.node())) {
          // TODO complete?
          context().SetConstant(dep_node.node(), *arg);
        }

        size_t i =
            *ASSERT_NOT_NULL(node->params().at_or_null(dep_node.node()->id()));
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
    auto qt = VerifyType(d.value.get());
    if (qt.ok()) {
      type_params.append(d.name, qt, d.flags);
    } else {
      err = true;
    }
  }
  if (err) { return std::nullopt; }
  return type_params;
}

std::optional<core::Arguments<type::Typed<ir::Value>>> Compiler::VerifyArguments(
    core::Arguments<ast::Expression const *> const &args) {
  bool err      = false;
  auto arg_vals = args.Transform([&](ast::Expression const *expr) {
    auto expr_qual_type = VerifyType(expr);
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
  pos_args.emplace_back(operand);
  return type::QualType(type::MakeOverloadSet(std::move(member_types))
                            ->return_types(core::Arguments<type::Typed<ir::Value>>(
                                std::move(pos_args), {})),
                        type::Quals::Unqualified());
}

// TODO: Replace `symbol` with an enum.
type::QualType Compiler::VerifyBinaryOverload(
    std::string_view symbol, ast::Expression const *node,
    type::Typed<ir::Value> const &lhs, type::Typed<ir::Value> const &rhs) {
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
  pos_args.emplace_back(lhs);
  pos_args.emplace_back(rhs);
  return context().set_qual_type(
      node,
      type::QualType(type::MakeOverloadSet(std::move(member_types))
                         ->return_types(core::Arguments<type::Typed<ir::Value>>(
                             std::move(pos_args), {})),
                     type::Quals::Unqualified()));
}

std::pair<type::QualType,
          absl::flat_hash_map<ast::Expression const *, type::Callable const *>>
Compiler::VerifyCallee(ast::Expression const *callee,
                       core::Arguments<type::Typed<ir::Value>> const &args) {
  using return_type =
      std::pair<type::QualType, absl::flat_hash_map<ast::Expression const *,
                                                    type::Callable const *>>;
  ASSIGN_OR(return return_type(type::QualType::Error(), {}),  //
                   auto qt, VerifyType(callee));

  ASSIGN_OR(return return_type(qt, {}),  //
                   auto const &callable, qt.type()->if_as<type::Callable>());

  absl::flat_hash_map<ast::Expression const *, type::Callable const *>
      overload_map;
  for (auto const *overload : context().AllOverloads(callee).members()) {
    overload_map.emplace(
        overload, &qual_type_of(overload).value().type()->as<type::Callable>());
  }
  return return_type(qt, std::move(overload_map));
}

base::expected<type::QualType, Compiler::CallError> Compiler::VerifyCall(
    ast::Call const *call_expr,
    absl::flat_hash_map<ast::Expression const *, type::Callable const *> const
        &overload_map,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  LOG("VerifyCall", "%s", call_expr->DebugString());
  CallError errors;
  std::vector<std::tuple<ast::Expression const *, type::Callable const *,
                         core::Params<type::QualType>>>
      overload_params;

  // TODO: Is it possible that the returned references in `AllOverloads` is
  // invalidated during some computation of `ExtractParams`? Maybe if something
  // else is inserted into the map. I believe not even if something is inserted
  // the iterator into members is still valid because there's an extra layer of
  // indirection in the overload set. Do we really want to rely on this?!
  for (auto const *callee :
       context().AllOverloads(call_expr->callee()).members()) {
    auto maybe_qt = qual_type_of(callee);
    ExtractParams(callee, &maybe_qt.value().type()->as<type::Callable>(), args,
                  overload_params, errors);
  }

  // TODO: Expansion is relevant too.
  std::vector<std::vector<type::Type>> return_types;

  // TODO: Take a type::Typed<ir::Value> instead.
  auto args_qt = args.Transform([](auto const &typed_value) {
    return typed_value->empty()
               ? type::QualType::NonConstant(typed_value.type())
               : type::QualType::Constant(typed_value.type());
  });

  ast::OverloadSet os;
  for (auto const &expansion : ExpandedArguments(args_qt)) {
    for (auto const &[callee, callable_type, params] : overload_params) {
      // TODO: Assuming this is unambiguously callable is a bit of a stretch.

      // TODO: `core::IsCallable` already does this but doesn't give us access
      // to writing errors. Rewriting it here and then we'll look at how to
      // combine it later.
      for (size_t i = 0; i < expansion.pos().size(); ++i) {
        if (not type::CanCastImplicitly(expansion[i], params[i].value.type())) {
          // TODO: Currently as soon as we find an error with a call we move on.
          // It'd be nice to extract all the error information for each.
          errors.reasons.emplace(callable_type,
                                 Compiler::CallError::TypeMismatch{
                                     .parameter     = i,
                                     .argument_type = expansion[i],
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
          errors.reasons.emplace(callable_type,
                                 Compiler::CallError::TypeMismatch{
                                     .parameter     = param.name,
                                     .argument_type = expansion[param.name],
                                 });
          goto next_overload;
        }
      }

      os.insert(callee);

      return_types.push_back(callable_type->return_types(args));
      goto next_expansion;
    next_overload:;
    }

    return errors;
  next_expansion:;
  }

  context().SetViableOverloads(call_expr->callee(), std::move(os));

  ASSERT(return_types.size() == 1u);
  return type::QualType(return_types.front(), type::Quals::Unqualified());
}

}  // namespace compiler
