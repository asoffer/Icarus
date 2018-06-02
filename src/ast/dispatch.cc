#include "ast/dispatch.h"

#include "ast/function_literal.h"
#include "context.h"
#include "ir/func.h"
#include "scope.h"
#include "type/function.h"
#include "type/tuple.h"
#include "type/variant.h"

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

namespace AST {
static std::optional<std::pair<FnArgs<const type::Type *>, Binding>>
DispatchEntry(Expression *expr, const FnArgs<Expression *> &args,
              Context *ctx) {
  // TODO check if anything is called with named args (it's illegal to do so).
  // Also break up MakeUntyped so we don't need to check the named args section.
  auto maybe_binding =
      Binding::MakeUntyped(expr, args, {/* intentionally no lookup */});
  if (!maybe_binding) { return std::nullopt; }
  auto binding = std::move(maybe_binding).value();

  FnArgs<const type::Type *> call_arg_types;
  call_arg_types.pos_.resize(args.pos_.size(), nullptr);
  const auto &fn_opt_input = expr->type->as<type::Function>().input;
  ASSERT(binding.exprs_.size() == fn_opt_input.size());
  for (size_t i = 0; i < binding.exprs_.size(); ++i) {
    const type::Type *match =
        type::Meet(binding.exprs_.at(i).second->type, fn_opt_input.at(i));
    if (match == nullptr) { return std::nullopt; }
    binding.exprs_.at(i).first = fn_opt_input.at(i);

    ASSERT(i < call_arg_types.pos_.size());
    call_arg_types.pos_.at(i) = match;
  }

  return std::pair(std::move(call_arg_types), std::move(binding));
}

static std::optional<std::pair<FnArgs<const type::Type *>, Binding>>
ConstantDispatchEntry(Expression *expr, const FnArgs<Expression *> &args,
                      Context *ctx) {
  GeneratedFunction *fn = nullptr;
  {
    auto vals = Evaluate(expr, ctx);
    if (vals.empty() || vals[0] == IR::Val::None()) { return std::nullopt; }
    fn = ASSERT_NOT_NULL(std::get<IR::Func *>(vals[0].value)->gened_fn_);
  }

  auto maybe_binding = Binding::MakeUntyped(fn, args, fn->lookup_);
  if (!maybe_binding) { return std::nullopt; }
  auto binding = std::move(maybe_binding).value();

  FnArgs<const type::Type *> call_arg_types;
  call_arg_types.pos_.resize(args.pos_.size(), nullptr);
  for (const auto & [ key, val ] : fn->lookup_) {
    if (val < args.pos_.size()) { continue; }
    call_arg_types.named_.emplace(key, nullptr);
  }

  const auto &fn_opt_input = expr->type->as<type::Function>().input;
  for (size_t i = 0; i < binding.exprs_.size(); ++i) {
    if (binding.defaulted(i)) {
      if (fn->inputs[i]->IsDefaultInitialized()) { return std::nullopt; }
      binding.exprs_[i].first = fn_opt_input[i];
    } else {
      const type::Type *match =
          type::Meet(binding.exprs_[i].second->type, fn_opt_input[i]);
      if (match == nullptr) { return std::nullopt; }
      binding.exprs_[i].first = fn_opt_input[i];

      if (i < call_arg_types.pos_.size()) {
        call_arg_types.pos_[i] = match;
      } else {
        auto iter = call_arg_types.find(fn->inputs[i]->identifier->token);
        ASSERT(iter != call_arg_types.named_.end());
        iter->second = match;
      }
    }
  }

  return std::pair(std::move(call_arg_types), std::move(binding));
}

void DispatchTable::TryInsert(Expression *fn_option,
                              const FnArgs<Expression *> &args, Context *ctx) {
  // TODO the reason you fail to generate what you want is because right
  // here you are calling evaluate which is problematic for recursive
  // functions
  fn_option->VerifyType(ctx);
  if (fn_option->type->is<type::Function>()) {
    std::optional<std::pair<FnArgs<const type::Type *>, Binding>>
        dispatch_entry;

    if (fn_option->lvalue == Assign::Const) {
      dispatch_entry = ConstantDispatchEntry(fn_option, args, ctx);
    } else {
      dispatch_entry = DispatchEntry(fn_option, args, ctx);
    }
    if (!dispatch_entry.has_value()) { return; }
    auto[call_arg_types, binding] = *dispatch_entry;
    this->insert(std::move(call_arg_types), std::move(binding));

  } else if (fn_option->type == type::Generic) {
    auto vals = Evaluate(fn_option, ctx);
    if (vals.empty() || vals[0] == IR::Val::None()) { return; }
    auto &fn_val = vals[0].value;

    auto *fn = std::get<AST::Function *>(fn_val);
    if (auto binding = fn->ComputeType(args, ctx); binding.fn_expr_) {
      auto &gened_fn = binding.fn_expr_->as<GeneratedFunction>();
      // TODO this is copied almost exactly from above.
      FnArgs<const type::Type *> call_arg_types;
      call_arg_types.pos_.resize(args.pos_.size(), nullptr);
      for (const auto & [ key, val ] : fn->lookup_) {
        if (val < args.pos_.size()) { continue; }
        call_arg_types.named_.emplace(key, nullptr);
      }

      const auto &fn_opt_input = gened_fn.type->as<type::Function>().input;
      for (size_t i = 0; i < binding.exprs_.size(); ++i) {
        if (binding.defaulted(i)) {
          if (gened_fn.inputs[i]->IsDefaultInitialized()) { return; }
          binding.exprs_[i].first = fn_opt_input[i];
        } else {
          const type::Type *match =
              type::Meet(binding.exprs_[i].second->type, fn_opt_input[i]);
          if (match == nullptr) { return; }
          binding.exprs_[i].first = fn_opt_input[i];

          if (i < call_arg_types.pos_.size()) {
            call_arg_types.pos_[i] = match;
          } else {
            auto iter =
                call_arg_types.find(gened_fn.inputs[i]->identifier->token);
            ASSERT(iter != call_arg_types.named_.end());
            iter->second = match;
          }
        }
      }

      this->insert(std::move(call_arg_types), std::move(binding));
    } else {
      return;
    }
  } else if (fn_option->type == type::Err) {
    // If there's a type error, do I want to exit entirely or assume this
    // one doesn't exist? and just goto next_option?
    return;

  } else {
    UNREACHABLE(fn_option);
  }
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
      table.TryInsert(decl->identifier.get(), args, ctx);
    }
  }

  const type::Type *ret_type = ComputeRetType(args, table, ctx);
  return std::pair{std::move(table), ret_type};
}

std::pair<DispatchTable, const type::Type *> DispatchTable::Make(
    const FnArgs<Expression *> &args, Expression *fn, Context *ctx) {
  DispatchTable table;
  table.TryInsert(fn, args, ctx);
  const type::Type *ret_type = ComputeRetType(args, table, ctx);
  return std::pair{std::move(table), ret_type};
}

void DispatchTable::insert(FnArgs<const type::Type *> call_arg_types,
                           Binding binding, size_t expanded_size) {
  if (expanded_size == std::numeric_limits<size_t>::max()) {
    expanded_size = 1;
    call_arg_types.Apply([&expanded_size](const type::Type *t) {
      if (t->is<type::Variant>()) {
        expanded_size *= t->as<type::Variant>().size();
      }
    });
  }

  total_size_ += expanded_size;
  bindings_.emplace(std::move(call_arg_types), std::move(binding));
}

std::optional<Binding> Binding::MakeUntyped(
    AST::Expression *fn_expr, const FnArgs<Expression *> &args,
    const std::unordered_map<std::string, size_t> &index_lookup) {
  // index_lookup.size() works for constants because it considers default args.
  // It's empty for non-constants, but the other approach covers this because
  // default args are not allowed.
  Binding result(fn_expr, std::max(index_lookup.size(),
                                   args.pos_.size() + args.named_.size()));
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    result.exprs_[i] = std::pair(nullptr, args.pos_[i]);
  }

  // Match the named arguments
  for (const auto & [ name, expr ] : args.named_) {
    // TODO emit an error explaining why we couldn't use this one if there
    // was a missing named argument.
    auto iter = index_lookup.find(name);
    if (iter == index_lookup.end()) { return std::nullopt; }
    result.exprs_.at(iter->second) = std::pair(nullptr, expr);
  }
  return result;
}
}  // namespace AST
