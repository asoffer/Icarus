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

      } else if (auto *match = type::Meet(
                     ctx->type_of(binding->exprs_.at(i).get()), input_type);
                 match == nullptr) {
        return std::nullopt;
      }
    }

    if (fn->inputs[i]->const_) {
      bc.constants_.emplace(
          fn->inputs[i].get(),
          (binding->defaulted(i)
               ? backend::Evaluate(fn->inputs[i].get(), ctx)
               : backend::Evaluate(binding->exprs_.at(i).get(), ctx))[0]);
    }

    binding->exprs_.at(i).set_type(input_type);
  }
  return bc;
}

struct DispatchTableRow {
  bool SetTypes(type::Typed<FunctionLiteral *, type::Function> fn,
                Context *ctx);

  static std::optional<DispatchTableRow> Make(
      type::Typed<Expression *, type::Callable> fn_option,
      FnArgs<Expression *> const &args, Context *ctx);

  FnArgs<type::Type const *> call_arg_types_;
  Binding binding_;
  bool const_;

 private:
  DispatchTableRow(Binding b) : binding_(std::move(b)) {}
};

bool DispatchTableRow::SetTypes(type::Typed<FunctionLiteral *, type::Function> fn,
                             Context *ctx) {
  auto const &input_types    = fn.type()->input;
  bool bound_at_compile_time = (fn.get() != nullptr);
  for (size_t i = 0; i < binding_.exprs_.size(); ++i) {
    if (bound_at_compile_time && binding_.defaulted(i)) {
      if (fn.get()->inputs[i]->IsDefaultInitialized()) { return false; }
      binding_.exprs_.at(i).set_type(input_types.at(i));
      continue;
    }

    const type::Type *match =
        type::Meet(ctx->type_of(binding_.exprs_.at(i).get()), input_types.at(i));
    if (match == nullptr) { return false; }

    binding_.exprs_.at(i).set_type(input_types.at(i));

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

static bool IsConstant(Expression *e) {
  return e->is<Declaration>() && e->as<Declaration>().const_;
}

std::optional<DispatchTableRow> DispatchTableRow::Make(
    type::Typed<Expression *, type::Callable> fn_option,
    FnArgs<Expression *> const &args, Context *ctx) {
  if (IsConstant(fn_option.get())) {

    LOG << "const";
  } else {
    ASSERT(args.named_.size() == 0u);
    ASSERT(fn_option.type(), Is<type::Function>());
    ASSERT(args.pos_.size() ==
           fn_option.type()->as<type::Function>().input.size());

    Binding binding(fn_option, args.pos_.size());
    binding.SetPositionalArgs(args);
    for (size_t i = 0; i < args.pos_.size(); ++i) {
      binding.exprs_.at(i).set_type(ctx->type_of(args.pos_[i]));
    }

    binding.const_ = false;
    DispatchTableRow dispatch_table_row(std::move(binding));
    dispatch_table_row.call_arg_types_.pos_.resize(args.pos_.size(), nullptr);
    for (size_t i = 0;i < args.pos_.size(); ++i) {
      dispatch_table_row.call_arg_types_.pos_[i] = ctx->type_of(args.pos_[i]);
    }
    return dispatch_table_row;
  }

  *fn_option = std::visit(
      base::overloaded{
          [](IR::Func *fn) -> Expression * {
    return fn->gened_fn_; },
          [](FunctionLiteral *fn) -> Expression * {
    return fn; },
          [](IR::ForeignFn fn) -> Expression * {
    return fn.expr_; },
          [](auto &&) -> Expression * {
    UNREACHABLE(); }},
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

  binding.const_ = true;
  DispatchTableRow dispatch_table_row(std::move(binding));
  dispatch_table_row.call_arg_types_.pos_.resize(args.pos_.size(), nullptr);
  if (fn_option.get()->is<FunctionLiteral>()) {
    auto *generic_fn = &fn_option.get()->as<FunctionLiteral>();

    // TODO these are being ignored, which is definitely wrong for generics, but
    // we need to redo those anyway.
    auto bound_constants =
        ComputeBoundConstants(generic_fn, args, &dispatch_table_row.binding_, ctx);

    if (!bound_constants) { return std::nullopt; }
    ctx->mod_->to_complete_.emplace(*std::move(bound_constants), generic_fn);

    // TODO can generate fail? Probably
    *dispatch_table_row.binding_.fn_= generic_fn;
  }

  FunctionLiteral *fn = nullptr;

  if (dispatch_table_row.binding_.fn_.get()->is<FunctionLiteral>()) {
    fn = &dispatch_table_row.binding_.fn_.get()->as<FunctionLiteral>();
    for (const auto & [ key, val ] : fn->lookup_) {
      if (val < args.pos_.size()) { continue; }
      dispatch_table_row.call_arg_types_.named_.emplace(key, nullptr);
    }
  }

  if (!fn) { NOT_YET(fn); }

  if (fn_option.type() == type::Generic) {
    base::vector<type::Type const *> inputs, outputs;
    for (auto &in : fn->inputs) { inputs.push_back(ctx->type_of(in.get())); }
    LOG << inputs;
    for (auto &out : fn->outputs) {
      outputs.push_back(ctx->type_of(out.get()));
    }
    LOG << outputs;
    // LOG << bound_constants;
  }

  ASSERT(fn_option.type(), Is<type::Function>());
  auto f = type::Typed<FunctionLiteral *, type::Function>(fn, fn_option.type());
  if (!dispatch_table_row.SetTypes(f, ctx)) { return std::nullopt; }

  return dispatch_table_row;
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

static size_t ComputeExpansion(
    FnArgs<type::Type const *> const &call_arg_types) {
  size_t expanded_size = 1;
  call_arg_types.Apply([&expanded_size](const type::Type *t) {
    if (t->is<type::Variant>()) {
      expanded_size *= t->as<type::Variant>().size();
    }
  });
  return expanded_size;
}

std::pair<DispatchTable, type::Type const *> DispatchTable::Make(
    FnArgs<Expression *> const &args, OverloadSet const &overload_set,
    Context *ctx) {
  DispatchTable table;

  for (auto &overload : overload_set) {
    ASSERT(overload.type() != nullptr);
    auto maybe_dispatch_table_row = DispatchTableRow::Make(overload, args, ctx);
    if (!maybe_dispatch_table_row.has_value()) { continue; }
    table.total_size_ +=
        ComputeExpansion(maybe_dispatch_table_row->call_arg_types_);
    table.bindings_.emplace(std::move(maybe_dispatch_table_row->call_arg_types_),
                            std::move(maybe_dispatch_table_row->binding_));
  }

  type::Type const *ret_type = ComputeRetType(overload_set);

  return std::pair{std::move(table), ret_type};
}

void Binding::SetPositionalArgs(const FnArgs<Expression *> &args) {
  ASSERT(exprs_.size() >= args.pos_.size());
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    exprs_[i] = type::Typed<Expression *>(args.pos_.at(i), nullptr);
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
    exprs_.at(iter->second) = type::Typed<Expression *>(expr, nullptr);
  }
  return true;
}
}  // namespace AST
