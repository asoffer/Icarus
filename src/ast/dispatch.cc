#include "ast/dispatch.h"

#include "ast/function_literal.h"
#include "backend/eval.h"
#include "context.h"
#include "ir/func.h"
#include "scope.h"
#include "type/function.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace AST {
// Attempts to match the call argument types to the dependent types here. If
// it can it materializes a function literal and returns a pointer to it.
// Otherwise, returns nullptr.
std::optional<BoundConstants> ComputeBoundConstants(
    Function *fn, const FnArgs<Expression *> &args, Binding *binding,
    Context *ctx) {
  Context new_ctx(ctx->mod_);
  BoundConstants bound_constants;
  new_ctx.bound_constants_ = &bound_constants;

  // TODO handle declaration order
  for (size_t i = 0; i < fn->inputs.size(); ++i) {
    fn->inputs[i]->VerifyType(&new_ctx);
    if (fn->inputs[i]->type == type::Err) { return std::nullopt; }

    if (binding->defaulted(i) && fn->inputs[i]->IsDefaultInitialized()) {
      // TODO maybe send back an explanation of why this didn't match. Or
      // perhaps continue to get better diagnostics?
      return std::nullopt;
    } else {
      if (fn->inputs[i]->const_ && !binding->defaulted(i) &&
          binding->exprs_[i].second->lvalue != Assign::Const) {
        return std::nullopt;
      }

      if (!binding->defaulted(i)) {
        if (auto *match =
                type::Meet(binding->exprs_[i].second->type, fn->inputs[i]->type);
            match == nullptr) {
          return std::nullopt;
        }
      }
    }

    if (fn->inputs[i]->const_) {
      bound_constants.emplace(
          fn->inputs[i]->identifier->token,
          (binding->defaulted(i)
               ? backend::Evaluate(fn->inputs[i].get(), &new_ctx)
               : backend::Evaluate(binding->exprs_[i].second, ctx))[0]);
    }

    binding->exprs_[i].first = fn->inputs[i]->type;
  }
  return bound_constants;
}

bool DispatchEntry::SetTypes(FuncContent *fn) {
  const auto &input_types = binding_.fn_expr_->type->as<type::Function>().input;
  bool bound_at_compile_time = (fn == nullptr);
  for (size_t i = 0; i < binding_.exprs_.size(); ++i) {
    if (bound_at_compile_time && binding_.defaulted(i)) {
      if (fn->inputs[i]->IsDefaultInitialized()) { return false; }
      binding_.exprs_.at(i).first = input_types.at(i);
      continue;
    }

    const type::Type *match =
        type::Meet(binding_.exprs_.at(i).second->type, input_types[i]);
    if (match == nullptr) { return false; }

    binding_.exprs_.at(i).first = input_types.at(i);

    if (i < call_arg_types_.pos_.size()) {
      call_arg_types_.pos_.at(i) = match;
    } else {
      if (bound_at_compile_time) {
        auto iter = call_arg_types_.find(fn->inputs[i]->identifier->token);
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
    Expression *fn_option, const FnArgs<Expression *> &args, Context *ctx) {
  Expression *bound_fn;
  size_t binding_size;
  if (fn_option->type->is<type::Function>()) {
    if (fn_option->lvalue == Assign::Const) {
      GeneratedFunction *fn = ASSERT_NOT_NULL(
          backend::EvaluateAs<IR::Func *>(fn_option, ctx)->gened_fn_);
      bound_fn = fn;
      binding_size =
          std::max(fn->lookup_.size(), args.pos_.size() + args.named_.size());
    } else {
      bound_fn     = fn_option;
      binding_size = args.pos_.size() + args.named_.size();
    }
  } else if (fn_option->type == type::Generic) {
    bound_fn = backend::EvaluateAs<Function *>(fn_option, ctx);
    binding_size = std::max(bound_fn->as<Function>().lookup_.size(),
                            args.pos_.size() + args.named_.size());
  } else {
    UNREACHABLE(fn_option);
  }

  Binding binding(bound_fn, binding_size);
  binding.SetPositionalArgs(args);

  if (fn_option->lvalue == Assign::Const) {
    if (!binding.SetNamedArgs(args, bound_fn->as<FuncContent>().lookup_)) {
      return std::nullopt;
    }
  }

  DispatchEntry dispatch_entry(std::move(binding));
  dispatch_entry.call_arg_types_.pos_.resize(args.pos_.size(), nullptr);

  GeneratedFunction *fn = nullptr;
  if (fn_option->lvalue == Assign::Const) {
    if (fn_option->type == type::Generic) {
      auto *generic_fn     = &bound_fn->as<Function>();
      auto bound_constants = ComputeBoundConstants(
          generic_fn, args, &dispatch_entry.binding_, ctx);
      if (!bound_constants) { return std::nullopt; }

      // TODO can generate fail? Probably
      dispatch_entry.binding_.fn_expr_ = ASSERT_NOT_NULL(
          generic_fn->generate(std::move(bound_constants).value(), ctx->mod_));
    }

    fn = &dispatch_entry.binding_.fn_expr_->as<GeneratedFunction>();

    for (const auto & [ key, val ] : fn->lookup_) {
      if (val < args.pos_.size()) { continue; }
      dispatch_entry.call_arg_types_.named_.emplace(key, nullptr);
    }
  }

  if (!dispatch_entry.SetTypes(fn)) { return std::nullopt; }
  return dispatch_entry;
}

static const type::Type *ComputeRetType(const FnArgs<Expression *> &args,
                                        const DispatchTable &table,
                                        Context *ctx) {
  std::vector<std::vector<const type::Type *>> out_types;
  out_types.reserve(table.bindings_.size());

  if (table.bindings_.size() == 0u) { return type::Err; }

  for (const auto & [ key, val ] : table.bindings_) {
    if (val.fn_expr_->type->is<type::Function>()) {
      out_types.push_back(val.fn_expr_->type->as<type::Function>().output);
    } else {
      UNREACHABLE(val.fn_expr_->type);
    }
  }

  ASSERT(!out_types.empty());
  // TODO Can I assume all the lengths are the same?
  std::vector<const type::Type *> var_outs;
  var_outs.reserve(out_types[0].size());
  for (size_t i = 0; i < out_types[0].size(); ++i) {
    std::vector<const type::Type *> types;
    types.reserve(out_types.size());
    for (const auto &out_type : out_types) { types.push_back(out_type[i]); }
    var_outs.push_back(type::Var(types));
  }

  return type::Tup(var_outs);
}

std::pair<DispatchTable, const type::Type *> DispatchTable::Make(
    const FnArgs<Expression *> &args, const std::string &token, Scope *scope,
    Context *ctx) {
  DispatchTable table;
  for (auto *scope_ptr = scope; scope_ptr; scope_ptr = scope_ptr->parent) {
    if (scope_ptr->shadowed_decls_.find(token) !=
        scope_ptr->shadowed_decls_.end()) {
      // The declaration of this identifier is shadowed so it will be
      // difficult to give meaningful error messages. Bailing out.
      // TODO Come up with a good way to give decent error messages anyway (at
      // least in some circumstances. For instance, there may be overlap here,
      // but it may not be relevant to the function call at hand. If, for
      // example, one call takes an A | B and the other takes a B | C, but
      // here we wish to validate a call for just a C, it's safe to do so
      // here.
      return {};
    }

    auto iter = scope_ptr->decls_.find(token);
    if (iter == scope_ptr->decls_.end()) { continue; }
    for (const auto &decl : iter->second) {
      decl->VerifyType(ctx);
      // TODO HANDLE_CYCLIC_DEPENDENCIES;
      if (decl->type == type::Err) { return {}; }
      if (auto maybe_dispatch_entry =
              DispatchEntry::Make(decl->identifier.get(), args, ctx)) {
        table.InsertEntry(std::move(maybe_dispatch_entry).value());
      }
    }
  }

  const type::Type *ret_type = ComputeRetType(args, table, ctx);
  return std::pair{std::move(table), ret_type};
}

std::pair<DispatchTable, const type::Type *> DispatchTable::Make(
    const FnArgs<Expression *> &args, Expression *fn, Context *ctx) {
  DispatchTable table;
  fn->VerifyType(ctx);
  if (fn->type == type::Err) { return {}; }
  if (auto maybe_dispatch_entry = DispatchEntry::Make(fn, args, ctx)) {
    table.InsertEntry(std::move(maybe_dispatch_entry).value());
  }
  const type::Type *ret_type = ComputeRetType(args, table, ctx);
  return std::pair{std::move(table), ret_type};
}

void DispatchTable::InsertEntry(DispatchEntry entry) {
  size_t expanded_size = 1;
  entry.call_arg_types_.Apply([&expanded_size](const type::Type *t) {
    if (t->is<type::Variant>()) {
      expanded_size *= t->as<type::Variant>().size();
    }
  });

  total_size_ += expanded_size;
  bindings_.emplace(std::move(entry.call_arg_types_),
                    std::move(entry.binding_));
}

void Binding::SetPositionalArgs(const FnArgs<Expression *> &args) {
  ASSERT(exprs_.size() <= args.pos_.size());
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    exprs_[i] = std::pair(nullptr, args.pos_[i]);
  }
}

bool Binding::SetNamedArgs(
    const FnArgs<Expression *> &args,
    const std::unordered_map<std::string, size_t> &index_lookup) {
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
