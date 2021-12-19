#include "compiler/verify/common.h"

#include <optional>
#include <string_view>

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

void ExtractParams(
    Compiler &compiler, ast::Expression const *callee, type::Type t,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &args,
    std::vector<std::tuple<ast::Expression const *, type::ReturningType const *,
                           core::Params<type::QualType>>> &overload_params,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>
        &errors) {
  if (auto const *f = t.if_as<type::ReturningType>()) {
    auto result = core::Callability(f->params(), args,
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
      ExtractParams(compiler, callee, overload, args, overload_params, errors);
    }
    if (overload_params.empty()) { return; }
  } else if (auto const *gf = t.if_as<type::Generic<type::Function>>()) {
    auto const *i = gf->Instantiate(compiler.work_resources(), args);
    if (not i) { return; }
    overload_params.emplace_back(callee, i, i->params());
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

void AddOverloads(Context const &context, PersistentResources const &resources,
                  ast::Expression const *callee) {
  auto const *overloads = context.AllOverloads(callee);
  if (not overloads) { return; }
  Context const &context_root = context.root();
  for (auto const *overload : overloads->members()) {
    LOG("AddOverloads", "Callee: %p %s", overload, overload->DebugString());
    Context const &overload_root = overload->scope()
                                       ->Containing<ast::ModuleScope>()
                                       ->module()
                                       ->as<CompiledModule>()
                                       .context();
    // TODO: This is fraught, because we still don't have access to instantiated
    // contexts if that's what's needed here.
    type::QualType qt = (&context_root == &overload_root)
                            ? context.qual_types(overload)[0]
                            : overload_root.qual_types(overload)[0];
  }
}

}  // namespace

module::BasicModule const *DefiningModule(type::Type t) {
  if (auto const *s = t.if_as<type::Struct>()) { return s->defining_module(); }
  if (auto const *e = t.if_as<type::Enum>()) { return e->defining_module(); }
  if (auto const *f = t.if_as<type::Flags>()) { return f->defining_module(); }
  if (auto const *o = t.if_as<type::Opaque>()) { return o->defining_module(); }
  if (auto const *p = t.if_as<type::Pointer>()) {
    return DefiningModule(p->pointee());
  }
  if (auto const *a = t.if_as<type::Array>()) {
    return DefiningModule(a->data_type());
  }
  return nullptr;
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

std::optional<core::Arguments<type::Typed<ir::CompleteResultRef>>>
Compiler::VerifyArguments(absl::Span<ast::Call::Argument const> args,
                          ir::CompleteResultBuffer &out) {
  bool err = false;
  std::vector<std::tuple<type::Type, ssize_t>> refs;
  for (auto const &arg : args) {
    auto expr_qual_type = VerifyType(&arg.expr())[0];
    err |= not expr_qual_type.ok();
    if (err) {
      LOG("VerifyArguments", "Error with: %s", arg.expr().DebugString());
      refs.emplace_back(nullptr, -1);
    } else {
      LOG("VerifyArguments", "constant: %s", arg.expr().DebugString());
      if (expr_qual_type.constant()) {
        if (auto maybe_result = EvaluateToBufferOrDiagnose(
                type::Typed(&arg.expr(), expr_qual_type.type()))) {
          LOG("VerifyArguments", "%s",
              expr_qual_type.type().Representation((*maybe_result)[0]));
          out.append((*maybe_result)[0]);
          refs.emplace_back(expr_qual_type.type(), out.num_entries() - 1);
        }
      } else {
        refs.emplace_back(expr_qual_type.type(), out.num_entries() - 1);
      }
    }
  }

  if (err) { return std::nullopt; }

  core::Arguments<type::Typed<ir::CompleteResultRef>> arg_vals;

  size_t i = 0;
  for (auto const &[t, index] : refs) {
    absl::Cleanup c = [&] { ++i; };
    auto ref        = index == -1 ? ir::CompleteResultRef() : out[index];
    if (not args[i].named()) {
      arg_vals.pos_emplace(ref, t);
    } else {
      arg_vals.named_emplace(args[i].name(), type::Typed(ref, t));
    }
  }

  return arg_vals;
}

// TODO: Replace `symbol` with an enum.
type::QualType Compiler::VerifyUnaryOverload(
    char const *symbol, ast::Expression const *node,
    type::Typed<ir::CompleteResultRef> const &operand) {
  absl::flat_hash_set<type::Function const *> member_types;

  node->scope()->ForEachDeclIdTowardsRoot(
      symbol, [&](ast::Declaration::Id const *id) {
        ASSIGN_OR(return false, auto qt, context().qual_types(id)[0]);
        // Must be callable because we're looking at overloads for operators
        // which have previously been type-checked to ensure callability.
        auto &c = qt.type().as<type::Function>();
        member_types.insert(&c);
        return true;
      });

  if (member_types.empty()) {
    return context().set_qual_type(node, type::QualType::Error())[0];
  }

  ASSERT(member_types.size() == 1u);
  // TODO: Check that we only have one return type on each of these overloads.
  return type::QualType((*member_types.begin())->return_types()[0],
                        type::Quals::Unqualified());
}

type::QualType Compiler::VerifyBinaryOverload(
    std::string_view symbol, ast::Expression const *node,
    type::Typed<ir::CompleteResultRef> const &lhs,
    type::Typed<ir::CompleteResultRef> const &rhs) {
  absl::flat_hash_set<type::Function const *> member_types;

  node->scope()->ForEachDeclIdTowardsRoot(
      symbol, [&](ast::Declaration::Id const *id) {
        ASSIGN_OR(return false, auto qt, context().qual_types(id)[0]);
        // Must be callable because we're looking at overloads for operators
        // which have previously been type-checked to ensure callability.
        auto &c = qt.type().as<type::Function>();
        member_types.insert(&c);
        return true;
      });

  if (member_types.empty()) { return type::QualType::Error(); }
  ASSERT(member_types.size() == 1u);
  // TODO: Check that we only have one return type on each of these overloads.
  return type::QualType((*member_types.begin())->return_types()[0],
                        type::Quals::Unqualified());
}

type::QualType Compiler::VerifyCallee(
    ast::Expression const *callee,
    absl::flat_hash_set<type::Type> const &argument_dependent_lookup_types) {
  LOG("VerifyCallee", "Verify callee: %s", callee->DebugString());

  // Set modules to be used for ADL before calling VerifyType on the callee, so
  // the verifier knows which contexts to look things up in.
  if (auto const *id = callee->if_as<ast::Identifier>()) {
    absl::flat_hash_set<compiler::CompiledModule const *> adl_modules;
    for (type::Type t : argument_dependent_lookup_types) {
      if (auto const *mod = type::Provenance(t)) {
        if (mod == resources().module) { continue; }
        adl_modules.insert(&mod->as<compiler::CompiledModule>());
      }
    }

    context().SetAdlModules(id, std::move(adl_modules));
  }

  ASSIGN_OR(return type::QualType::Error(),  //
                   auto qt, VerifyType(callee)[0]);

  ASSIGN_OR(return qt,  //
                   auto const &callable, qt.type().if_as<type::Callable>());

  AddOverloads(context(), resources(), callee);
  for (type::Type t : argument_dependent_lookup_types) {
    // TODO: Generic structs? Arrays? Pointers?
    if (auto const *s = t.if_as<type::Struct>()) {
      AddOverloads(
          s->defining_module()->as<compiler::CompiledModule>().context(),
          resources(), callee);
    }
  }

  return qt;
}

std::variant<
    std::vector<type::QualType>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
Compiler::VerifyCall(
    ast::Call const *call_expr,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &args) {
  LOG("VerifyCall", "%s", call_expr->DebugString());
  absl::flat_hash_map<type::Callable const *, core::CallabilityResult> errors;
  std::vector<std::tuple<ast::Expression const *, type::ReturningType const *,
                         core::Params<type::QualType>>>
      overload_params;

  // TODO: Is it possible that the returned references in `AllOverloads` is
  // invalidated during some computation of `ExtractParams`? Maybe if something
  // else is inserted into the map. I believe not even if something is inserted
  // the iterator into members is still valid because there's an extra layer of
  // indirection in the overload set. Do we really want to rely on this?!
  if (auto const *overloads = context().AllOverloads(call_expr->callee())) {
    Context const &context_root = context().root();
    for (auto const *callee : overloads->members()) {
      Context const &callee_root = callee->scope()
                                       ->Containing<ast::ModuleScope>()
                                       ->module()
                                       ->as<CompiledModule>()
                                       .context();
      // TODO: This is fraught, because we still don't have access to
      // instantiated contexts if that's what's needed here.
      auto &ctx = (&context_root == &callee_root) ? context() : callee_root;
      type::QualType qt = ctx.qual_types(callee)[0];
      ExtractParams(*this, callee, qt.type(), args, overload_params, errors);
    }
  }

  LOG("VerifyCall", "%u overloads", overload_params.size());

  // TODO: Expansion is relevant too.
  std::vector<std::vector<type::Type>> return_types;

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

      if (expansion.pos().size() > params.size()) { continue; }
      for (size_t i = 0; i < expansion.pos().size(); ++i) {
        LOG("VerifyCall", "Comparing parameter %s with argument %s",
            expansion[i].to_string(), params[i].value.type().to_string());
        if (not type::CanCastImplicitly(expansion[i], params[i].value.type())) {
          // TODO: Currently as soon as we find an error with a call we move on.
          // It'd be nice to extract all the error information for each.
          LOG("VerifyCall",
              "Cannot cast implicitly: parameter %s with argument %s",
              expansion[i].to_string(), params[i].value.type().to_string());

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
      if (not callable_type->is<type::Generic<type::Struct>>()) {
        quals &= ~type::Quals::Const();
      }
      return_types.emplace_back(callable_type->return_types().begin(),
                                callable_type->return_types().end());
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
    for (auto const &argument : yield_stmt->arguments()) {
      // TODO: Determine whether or not you want to support named yields. If
      // not, reduce this to a vector or some other positional arguments type.
      yielded.pos_emplace(context.qual_types(&argument.expr())[0]);
    }
  }
  return yield_types;
}

}  // namespace compiler
