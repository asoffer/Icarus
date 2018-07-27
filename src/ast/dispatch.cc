#include "ast/dispatch.h"

#include "ast/function_literal.h"
#include "ast/match_declaration.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "context.h"
#include "ir/func.h"
#include "scope.h"
#include "type/function.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace AST {
using base::check::Is;

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

    if ((binding->defaulted(i) && fn->inputs[i]->IsDefaultInitialized()) ||
        (fn->inputs[i]->const_ && !binding->defaulted(i) &&
         binding->exprs_[i].second->lvalue != Assign::Const)) {
      // TODO maybe send back an explanation of why this didn't match. Or
      // perhaps continue to get better diagnostics?
      return std::nullopt;
    }

    if (!binding->defaulted(i)) {
      if (fn->inputs[i]->type_expr != nullptr &&
          fn->inputs[i]->type_expr->type == type::Interface) {
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
        bound_constants.interfaces_.emplace(fn->inputs[i].get(),
                                            binding->exprs_[i].second->type);
        if (fn->inputs[i]->type_expr->is<MatchDeclaration>()) {
          bound_constants.constants_.emplace(
              fn->inputs[i]
                  ->type_expr->as<MatchDeclaration>()
                  .identifier->token,
              IR::Val::Type(binding->exprs_[i].second->type));
        }

        */
        // TODO using this for now to signify an interface when in reality we
        // want something much more meaningful. 'Generic' is a weird catch-all
        // type currently that needs to be deprecated.

      } else if (auto *match = type::Meet(binding->exprs_[i].second->type,
                                          fn->inputs[i]->type);
                 match == nullptr) {
        return std::nullopt;
      }
    }

    if (fn->inputs[i]->const_) {
      bound_constants.constants_.emplace(
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
  ASSERT(binding_.fn_expr_->type, Is<type::Function>());
  const auto &input_types = binding_.fn_expr_->type->as<type::Function>().input;
  bool bound_at_compile_time = (fn != nullptr);
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
  Expression *bound_fn = nullptr;
  size_t binding_size;
  if (fn_option->lvalue == Assign::Const) {
    bound_fn = std::visit(
        base::overloaded{
            [](IR::Func *fn) -> Expression * { return fn->gened_fn_; },
            [](Function *fn) -> Expression * { return fn; },
            [](IR::ForeignFn fn) -> Expression * { return fn.expr_; },
            [](auto &&) -> Expression * { UNREACHABLE(); }},
        backend::Evaluate(fn_option, ctx)[0].value);

    if (bound_fn->is<FuncContent>()) {
      binding_size = std::max(bound_fn->as<FuncContent>().lookup_.size(),
                              args.pos_.size() + args.named_.size());
    } else {
      // TODO must this be a builtin?
      // TODO is this 1 even right?
      binding_size = 1;
    }
  } else {
    bound_fn     = fn_option;
    binding_size = args.pos_.size() + args.named_.size();
  }
  Binding binding(bound_fn, binding_size);
  binding.SetPositionalArgs(args);

  if (fn_option->lvalue == Assign::Const) {
    if (bound_fn->is<FuncContent>() &&
        !binding.SetNamedArgs(args, bound_fn->as<FuncContent>().lookup_)) {
      return std::nullopt;
    }
  }

  DispatchEntry dispatch_entry(std::move(binding));
  dispatch_entry.call_arg_types_.pos_.resize(args.pos_.size(), nullptr);

  if (bound_fn->is<Function>()) {
    auto *generic_fn = &bound_fn->as<Function>();

    auto bound_constants =
        ComputeBoundConstants(generic_fn, args, &dispatch_entry.binding_, ctx);
    if (!bound_constants) { return std::nullopt; }

    // TODO can generate fail? Probably
    dispatch_entry.binding_.fn_expr_ = ASSERT_NOT_NULL(
        generic_fn->generate(std::move(bound_constants).value()));
  }

  FuncContent *fn = nullptr;
  if (fn_option->lvalue == Assign::Const) {
    if (dispatch_entry.binding_.fn_expr_->is<FuncContent>()) {
      fn = &dispatch_entry.binding_.fn_expr_->as<FuncContent>();
      for (const auto & [ key, val ] : fn->lookup_) {
        if (val < args.pos_.size()) { continue; }
        dispatch_entry.call_arg_types_.named_.emplace(key, nullptr);
      }
    }
  }

  if (!dispatch_entry.SetTypes(fn)) { return std::nullopt; }
  return dispatch_entry;
}

static const type::Type *ComputeRetType(const FnArgs<Expression *> &args,
                                        const DispatchTable &table,
                                        Context *ctx) {
  base::vector<base::vector<const type::Type *>> out_types;
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
    const FnArgs<Expression *> &args, const std::string &token, Scope *scope,
    Context *ctx) {
  DispatchTable table;
  // TODO error decls?
  auto[decls, error_decls] = scope->AllDeclsWithId(token, ctx);
  for (auto &decl : decls) {
    if (decl->type == type::Err) { return {}; }
    if (auto maybe_dispatch_entry =
            DispatchEntry::Make(decl->identifier.get(), args, ctx)) {
      table.InsertEntry(std::move(maybe_dispatch_entry).value());
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
