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
/*static std::optional<BoundConstants> ComputeBoundConstants(
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
}*/

struct DispatchTableRow {
  bool SetTypes(type::Typed<Expression *, type::Function> fn, Context *ctx);
  bool SetTypes(IR::Func const &fn, FnArgs<Expression *> const &args,
                Context *ctx);
  bool SetTypes(FunctionLiteral const &fn, FnArgs<Expression *> const &args,
                Context *ctx);
  static std::optional<DispatchTableRow> Make(
      type::Typed<Expression *, type::Callable> fn_option,
      FnArgs<Expression *> const &args, Context *ctx);

  FnArgs<type::Type const *> call_arg_types_;
  Binding binding_;

 private:
  static std::optional<DispatchTableRow> MakeNonConstant(
      type::Typed<Expression *, type::Function> fn_option,
      FnArgs<Expression *> const &args, Context *ctx);

  static std::optional<DispatchTableRow> MakeFromForeignFunction(
      type::Typed<Expression *, type::Callable> fn_option,
      FnArgs<Expression *> const &args, Context *ctx);
  static std::optional<DispatchTableRow> MakeFromIrFunc(
      type::Typed<Expression *, type::Callable> fn_option,
      IR::Func const &ir_func, FnArgs<Expression *> const &args, Context *ctx);

  static std::optional<DispatchTableRow> MakeFromFnLit(
      type::Typed<Expression *, type::Callable> fn_option,
      FunctionLiteral *fn_lit, FnArgs<Expression *> const &args,
      Context *ctx);
  DispatchTableRow(Binding b) : binding_(std::move(b)) {}
};

bool DispatchTableRow::SetTypes(type::Typed<Expression *, type::Function> fn,
                                   Context *ctx) {
  ASSERT(fn.type()->input.size() == binding_.exprs_.size());
  call_arg_types_.pos_.resize(fn.type()->input.size());
  auto const &input_types = fn.type()->input;
  for (size_t i = 0; i < binding_.exprs_.size(); ++i) {
    type::Type const *match = type::Meet(
        ctx->type_of(binding_.exprs_.at(i).get()), input_types.at(i));
    if (match == nullptr) { return false; }

    binding_.exprs_.at(i).set_type(input_types.at(i));
    call_arg_types_.pos_.at(i) = match;
  }

  return true;
}

bool DispatchTableRow::SetTypes(FunctionLiteral const &fn,
                                FnArgs<Expression *> const &args,
                                Context *ctx) {
  call_arg_types_.pos_.resize(args.pos_.size());
  for (auto & [ name, expr ] : args.named_) {
    call_arg_types_.named_.emplace(name, nullptr);
  }

  NOT_YET();
}

bool DispatchTableRow::SetTypes(IR::Func const &fn,
                                FnArgs<Expression *> const &args,
                                Context *ctx) {
  call_arg_types_.pos_.resize(args.pos_.size());
  for (auto & [ name, expr ] : args.named_) {
    call_arg_types_.named_.emplace(name, nullptr);
  }

  auto const &input_types = fn.type_->input;
  for (size_t i = 0; i < binding_.exprs_.size(); ++i) {
    if (binding_.defaulted(i)) {
      if (fn.args_.at(i).second == nullptr) { return false; }
      binding_.exprs_.at(i).set_type(input_types.at(i));
      continue;
    }

    type::Type const *match = type::Meet(
        ctx->type_of(binding_.exprs_.at(i).get()), input_types.at(i));
    if (match == nullptr) { return false; }

    binding_.exprs_.at(i).set_type(input_types.at(i));

    if (i < call_arg_types_.pos_.size()) {
      call_arg_types_.pos_.at(i) = match;
    } else {
      auto iter = call_arg_types_.find(fn.args_.at(i).first);
      ASSERT(iter != call_arg_types_.named_.end());
      iter->second = match;
    }
  }

  return true;
}

static bool IsConstant(Expression *e) {
  return e->is<FunctionLiteral>() ||
         (e->is<Declaration>() && e->as<Declaration>().const_);
}

std::optional<DispatchTableRow> DispatchTableRow::MakeNonConstant(
    type::Typed<Expression *, type::Function> fn_option,
    FnArgs<Expression *> const &args, Context *ctx) {
  if (!args.named_.empty()) {
    NOT_YET(
        "Log an explanation for which this option was disregarded. (named "
        "args)");
  }

  if (args.pos_.size() != fn_option.type()->input.size()) {
    NOT_YET(
        "Log an explanation for which this option was disregarded. (default "
        "args)");
  }

  Binding binding(fn_option, args.pos_.size(), false);
  binding.SetPositionalArgs(args);
  DispatchTableRow dispatch_table_row(std::move(binding));

  if (!dispatch_table_row.SetTypes(fn_option, ctx)) { return {}; }
  return dispatch_table_row;
}

std::optional<DispatchTableRow> DispatchTableRow::Make(
    type::Typed<Expression *, type::Callable> fn_option,
    FnArgs<Expression *> const &args, Context *ctx) {
  if (!IsConstant(fn_option.get())) {
    return MakeNonConstant(fn_option.as_type<type::Function>(), args, ctx);
  }

  IR::Val fn_val = backend::Evaluate(fn_option, ctx).at(0);
  if (auto *ff = std::get_if<IR::ForeignFn>(&fn_val.value)) {
    return MakeFromForeignFunction(fn_option, args, ctx);

  } else if (auto *ir_func = std::get_if<IR::Func *>(&fn_val.value)) {
    return MakeFromIrFunc(fn_option, **ir_func, args, ctx);

  } else if (auto *fn = std::get_if<FunctionLiteral*>(&fn_val.value)) {
    return MakeFromFnLit(fn_option, *fn, args, ctx);

  } else {
    UNREACHABLE();
  }
}

std::optional<DispatchTableRow> DispatchTableRow::MakeFromForeignFunction(
    type::Typed<Expression *, type::Callable> fn_option,
    FnArgs<Expression *> const &args, Context *ctx) {
  // TODO while all the behavior of MakeNonConst is what we want, the name is
  // obviously incorrect. and we need to reset binding_.const_ to true. Fix
  // the name here. Probably the error messages once we have them will be
  // wrong too.
  auto result = MakeNonConstant(fn_option.as_type<type::Function>(), args, ctx);
  if (!result) { return {}; }
  result->binding_.const_ = true;
  return result;
}

std::optional<DispatchTableRow> DispatchTableRow::MakeFromFnLit(
    type::Typed<Expression *, type::Callable> fn_option,
    FunctionLiteral *fn_lit, FnArgs<Expression *> const &args, Context *ctx) {
  size_t binding_size =
      std::max(fn_lit->inputs.size(), args.pos_.size() + args.named_.size());

  Binding binding(fn_option, binding_size, true);
  binding.SetPositionalArgs(args);
  if (!binding.SetNamedArgs(args, fn_lit->lookup_)) { return {}; }

  for (size_t i = 0; i < args.pos_.size(); ++i) {
    if (!fn_lit->inputs[i]->const_) { continue; }
    // TODO this is wrong because it needs to be removed outside the scope of
    // this function.
    ctx->bound_constants_.constants_.emplace(
        fn_lit->inputs[i].get(), backend::Evaluate(args.pos_[i], ctx)[0]);
  }
  // TODO named arguments too.
  fn_lit->VerifyTypeConcrete(ctx);
  fn_lit->Validate(ctx);

  DispatchTableRow dispatch_table_row(std::move(binding));
  if (!dispatch_table_row.SetTypes(*fn_lit, args, ctx)) { return {}; }
  return dispatch_table_row;
}

std::optional<DispatchTableRow> DispatchTableRow::MakeFromIrFunc(
    type::Typed<Expression *, type::Callable> fn_option,
    IR::Func const &ir_func, FnArgs<Expression *> const &args, Context *ctx) {
  size_t binding_size =
      std::max(ir_func.args_.size(), args.pos_.size() + args.named_.size());

  Binding binding(fn_option, binding_size, true);
  binding.SetPositionalArgs(args);
  if (!binding.SetNamedArgs(args, ir_func.lookup_)) { return {}; }

  DispatchTableRow dispatch_table_row(std::move(binding));
  if (!dispatch_table_row.SetTypes(ir_func, args, ctx)) { return {}; }
  return dispatch_table_row;
}

/*{
  auto *fn = &fn_option.get()->as<FunctionLiteral>();
  size_t binding_size =
      std::max(fn->lookup_.size(), args.pos_.size() + args.named_.size());

  Binding binding(fn_option, binding_size, true);
  binding.SetPositionalArgs(args);
  if (!binding.SetNamedArgs(args, fn->lookup_)) { return {}; }

  DispatchTableRow dispatch_table_row(std::move(binding));
  dispatch_table_row.call_arg_types_.pos_.resize(args.pos_.size(), nullptr);

  // TODO these are being ignored, which is definitely wrong for generics, but
  // we need to redo those anyway.
  auto bound_constants =
      ComputeBoundConstants(fn, args, &dispatch_table_row.binding_, ctx);

  if (!bound_constants) { return std::nullopt; }
  ctx->mod_->to_complete_.emplace(*std::move(bound_constants), fn);

  *dispatch_table_row.binding_.fn_ = fn;

  for (const auto & [ key, val ] : fn->lookup_) {
    if (val < args.pos_.size()) { continue; }
    dispatch_table_row.call_arg_types_.named_.emplace(key, nullptr);
  }

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
  NOT_YET();

  return dispatch_table_row;
}*/

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

  // TODO this won't work with generics. Need to get the info from the table
  // itself. Probably put in in a row.
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
