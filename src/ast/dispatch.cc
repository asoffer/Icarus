#include "ast/dispatch.h"

#include "ast/function_literal.h"
#include "ast/match_declaration.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "context.h"
#include "ir/func.h"
#include "module.h"
#include "scope.h"
#include "type/function.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace AST {
using base::check::Is;

// Attempts to match the call argument types to the dependent types here. If
// it can it materializes a function literal and returns a pointer to it.
// Otherwise, returns nullptr.
static std::optional<BoundConstants> ComputeBoundConstants(
    FunctionLiteral *fn, const FnArgs<Expression *> &args, Binding *binding,
    Context *ctx) {
  BoundConstants bc;
  // TODO handle declaration order
  for (size_t i = 0; i < fn->inputs.size(); ++i) {
    auto *input_type = ctx->type_of(fn->inputs[i].get());
    if (input_type == nullptr) { return std::nullopt; }

    if (binding->defaulted(i) && fn->inputs[i]->IsDefaultInitialized()) {
      // Tried to call using a default argument, but the function did not
      // provide a default.
      return std::nullopt;
    }

    if (!binding->defaulted(i)) {
      if (fn->inputs[i]->type_expr != nullptr &&
          ctx->type_of(fn->inputs[i]->type_expr.get()) == type::Interface) {
        // TODO case where it is defaulted.
        // TODO expand all variants
        NOT_YET();
        /*
        auto ifc = backend::EvaluateAs<IR::Interface>(
            fn->inputs[i]->type_expr.get(), ctx);
        auto errs = ifc.MatchErrors(binding->exprs_[i].second->type);
        if (!errs.empty()) {
          for (auto err : errs) { LOG << err; }
          return std::nullopt;
        }
        bc.interfaces_.emplace(fn->inputs[i].get(),
                               binding->exprs_[i].second->type);
        if (fn->inputs[i]->type_expr->is<MatchDeclaration>()) {
          bc.constants_.emplace(fn->inputs[i]
                                    ->type_expr->as<MatchDeclaration>()
                                    .identifier->token,
                                IR::Val(binding->exprs_[i].second->type));
        }

        */
        // TODO using this for now to signify an interface when in reality we
        // want something much more meaningful. 'Generic' is a weird catch-all
        // type currently that needs to be deprecated.

      } else if (auto *match = type::Meet(
                     ctx->type_of(binding->exprs_[i].second), input_type);
                match == nullptr) {
        return std::nullopt;
      }
    }

    if (fn->inputs[i]->const_) {
      bc.constants_.emplace(
          fn->inputs[i].get(),
          (binding->defaulted(i)
               ? backend::Evaluate(fn->inputs[i].get(), ctx)
               : backend::Evaluate(binding->exprs_[i].second, ctx))[0]);
    }

    binding->exprs_[i].first = input_type;
  }
  return bc;
}


// Represents a row in the dispatch table.
struct DispatchEntry {
  bool SetTypes(type::Typed<FunctionLiteral *, type::Function> fn,
                Context *ctx);

  static std::optional<DispatchEntry> Make(
      type::Typed<Expression *, type::Callable> fn_option,
      FnArgs<Expression *> const &args, Context *ctx);

  FnArgs<const type::Type *> call_arg_types_;
  Binding binding_;

 private:
  DispatchEntry(Binding b) : binding_(std::move(b)) {}
};

bool DispatchEntry::SetTypes(type::Typed<FunctionLiteral *, type::Function> fn,
                             Context *ctx) {
  auto const &input_types    = fn.type()->input;
  bool bound_at_compile_time = (fn.get() != nullptr);
  for (size_t i = 0; i < binding_.exprs_.size(); ++i) {
    if (bound_at_compile_time && binding_.defaulted(i)) {
      if (fn.get()->inputs[i]->IsDefaultInitialized()) { return false; }
      binding_.exprs_.at(i).first = input_types.at(i);
      continue;
    }

    const type::Type *match =
        type::Meet(ctx->type_of(binding_.exprs_.at(i).second), input_types.at(i));
    if (match == nullptr) { return false; }

    binding_.exprs_.at(i).first = input_types.at(i);

    if (i < call_arg_types_.pos_.size()) {
      call_arg_types_.pos_.at(i) = match;
    } else {
      if (bound_at_compile_time) {
        auto iter = call_arg_types_.find(fn.get()->inputs[i]->id_);
        ASSERT(iter != call_arg_types_.named_.end());
        iter->second = match;
      } else {
        UNREACHABLE();
      }
    }
  }
  return true;
}

std::optional<DispatchEntry> DispatchEntry::Make(
    type::Typed<Expression *, type::Callable> fn_option,
    const FnArgs<Expression *> &args, Context *ctx) {
  *fn_option = std::visit(
      base::overloaded{
          [](IR::Func *fn) -> Expression * { return fn->gened_fn_; },
          [](FunctionLiteral *fn) -> Expression * { return fn; },
          [](IR::ForeignFn fn) -> Expression * { return fn.expr_; },
          [](auto &&) -> Expression * { UNREACHABLE(); }},
      backend::Evaluate(fn_option, ctx).at(0).value);

  size_t binding_size;
  if (fn_option.get()->is<FunctionLiteral>()) {
    binding_size =
        std::max(fn_option.get()->as<FunctionLiteral>().lookup_.size(),
                 args.pos_.size() + args.named_.size());
  } else {
    // TODO must this be a builtin?
    // TODO is this 1 even right?
    binding_size = 1;
  }
  Binding binding(fn_option, binding_size);
  binding.SetPositionalArgs(args);

  if (auto *f = fn_option.get();
      f->is<FunctionLiteral>() &&
      !binding.SetNamedArgs(args, f->as<FunctionLiteral>().lookup_)) {
    return std::nullopt;
  }

  DispatchEntry dispatch_entry(std::move(binding));
  dispatch_entry.call_arg_types_.pos_.resize(args.pos_.size(), nullptr);

  if (fn_option.get()->is<FunctionLiteral>()) {
    auto *generic_fn = &fn_option.get()->as<FunctionLiteral>();

    // TODO these are being ignored, which is definitely wrong for generics, but
    // we need to redo those anyway.
    auto bound_constants =
        ComputeBoundConstants(generic_fn, args, &dispatch_entry.binding_, ctx);

    if (!bound_constants) { return std::nullopt; }
    ctx->mod_->to_complete_.emplace(*std::move(bound_constants), generic_fn);

    // TODO can generate fail? Probably
    *dispatch_entry.binding_.fn_= generic_fn;
  }

  FunctionLiteral *fn = nullptr;

  if (dispatch_entry.binding_.fn_.get()->is<FunctionLiteral>()) {
    fn = &dispatch_entry.binding_.fn_.get()->as<FunctionLiteral>();
    for (const auto & [ key, val ] : fn->lookup_) {
      if (val < args.pos_.size()) { continue; }
      dispatch_entry.call_arg_types_.named_.emplace(key, nullptr);
    }
  }

  ASSERT(fn_option.type(), Is<type::Function>());
  auto f = type::Typed<FunctionLiteral *, type::Function>(fn, fn_option.type());
  if (!dispatch_entry.SetTypes(f, ctx)) { return std::nullopt; }

  return dispatch_entry;
}

static const type::Type *ComputeRetType(OverloadSet const &overload_set) {
  base::vector<base::vector<const type::Type *>> out_types;
  for (auto const &overload : overload_set) {
    ASSERT(overload.type(), Is<type::Function>());
    out_types.push_back(overload.type()->as<type::Function>().output);
  }

  ASSERT(!out_types.empty());
  // TODO Can I assume all the lengths are the same?
  base::vector<const type::Type *> var_outs;
  var_outs.reserve(out_types[0].size());
  for (size_t i = 0; i < out_types[0].size(); ++i) {
    base::vector<const type::Type *> types;
    types.reserve(out_types.size());
    for (const auto &out_type : out_types) { types.push_back(out_type[i]); }
    var_outs.push_back(type::Var(types));
  }

  return type::Tup(var_outs);
}

std::pair<DispatchTable, const type::Type *> DispatchTable::Make(
    const FnArgs<Expression *> &args, OverloadSet const &overload_set,
    Context *ctx) {
  DispatchTable table;
  // TODO error decls?
  for (auto &fn : overload_set) {
    if (fn.type() == nullptr) {
      // TODO can this even happen?
      return {};
    }
    if (auto maybe_dispatch_entry = DispatchEntry::Make(fn, args, ctx)) {
      size_t expanded_size = 1;
      maybe_dispatch_entry->call_arg_types_.Apply(
          [&expanded_size](const type::Type *t) {
            if (t->is<type::Variant>()) {
              expanded_size *= t->as<type::Variant>().size();
            }
          });

      table.total_size_ += expanded_size;
      table.bindings_.emplace(std::move(maybe_dispatch_entry->call_arg_types_),
                              std::move(maybe_dispatch_entry->binding_));
    }
  }

  type::Type const *ret_type = ComputeRetType(overload_set);
  return std::pair{std::move(table), ret_type};
}

void Binding::SetPositionalArgs(const FnArgs<Expression *> &args) {
  ASSERT(exprs_.size() >= args.pos_.size());
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    exprs_[i] = std::pair(nullptr, args.pos_[i]);
  }
}

bool Binding::SetNamedArgs(
    const FnArgs<Expression *> &args,
    const base::unordered_map<std::string, size_t> &index_lookup) {
  for (const auto & [ name, expr ] : args.named_) {
    // TODO emit an error explaining why we couldn't use this one if there
    // was a missing named argument.
    auto iter = index_lookup.find(name);
    if (iter == index_lookup.end()) { return false; }
    exprs_.at(iter->second) = std::pair(nullptr, expr);
  }
  return true;
}
}  // namespace AST
