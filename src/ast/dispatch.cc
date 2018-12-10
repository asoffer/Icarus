#include "ast/dispatch.h"

#include "ast/function_literal.h"
#include "ast/match_declaration.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "context.h"
#include "ir/func.h"
#include "module.h"
#include "scope.h"
#include "type/cast.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace ast {
using base::check::Is;

struct DispatchTableRow {
  bool SetTypes(type::Typed<Expression *, type::Function> fn, Context *ctx);
  bool SetTypes(ir::Func const &fn, FnArgs<Expression *> const &args,
                Context *ctx);
  bool SetTypes(FunctionLiteral const &fn, FnArgs<Expression *> const &args,
                Context *ctx);
  static base::expected<DispatchTableRow> Make(
      type::Typed<Expression *, type::Callable> fn_option,
      FnArgs<Expression *> const &args, Context *ctx);

  FnArgs<type::Type const *> call_arg_types_;
  type::Callable const *callable_type_ = nullptr;
  Binding binding_;

 private:
  static base::expected<DispatchTableRow> MakeNonConstant(
      type::Typed<Expression *, type::Function> fn_option,
      FnArgs<Expression *> const &args, Context *ctx);

  static base::expected<DispatchTableRow> MakeFromForeignFunction(
      type::Typed<Expression *, type::Callable> fn_option,
      FnArgs<Expression *> const &args, Context *ctx);
  static base::expected<DispatchTableRow> MakeFromIrFunc(
      type::Typed<Expression *, type::Callable> fn_option,
      ir::Func const &ir_func, FnArgs<Expression *> const &args, Context *ctx);

  static base::expected<DispatchTableRow> MakeFromFnLit(
      type::Typed<Expression *, type::Callable> fn_option,
      FunctionLiteral *fn_lit, FnArgs<Expression *> const &args, Context *ctx);
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

  for (size_t i = 0; i < binding_.exprs_.size(); ++i) {
    if (binding_.defaulted(i)) {
      // The naming here is super confusing but if a declaration
      // "IsDefaultInitialized" that means it's of the form `foo: bar`, and so
      // it does NOT have a default value.
      if (fn.inputs.at(i)->IsDefaultInitialized()) { return false; }
      // TODO The order for evaluating these is wrong. Defaults may need to be
      // intermixed with non-defaults.
      fn.inputs.at(i).get()->VerifyType(ctx);
      fn.inputs.at(i).get()->Validate(ctx);
      ctx->bound_constants_.constants_.emplace(
          fn.inputs.at(i).get(),
          backend::Evaluate(fn.inputs.at(i)->init_val.get(), ctx)[0]);

      binding_.exprs_.at(i).set_type(ctx->type_of(fn.inputs.at(i).get()));
      continue;
    }
    // TODO defaulted arguments
    // TODO Meet with the argument
    type::Type const *match = ctx->type_of(fn.inputs.at(i).get());

    binding_.exprs_.at(i).set_type(match);
    if (i < call_arg_types_.pos_.size()) {
      call_arg_types_.pos_.at(i) = match;
    } else {
      auto iter = call_arg_types_.find(fn.inputs.at(i)->id_);
      ASSERT(iter != call_arg_types_.named_.end());
      iter->second = match;
    }
  }

  return true;
}

bool DispatchTableRow::SetTypes(ir::Func const &fn,
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

base::expected<DispatchTableRow> DispatchTableRow::MakeNonConstant(
    type::Typed<Expression *, type::Function> fn_option,
    FnArgs<Expression *> const &args, Context *ctx) {
  if (!args.named_.empty()) {
    // TODO Describe `fn_option` explicitly.
    return base::unexpected(
        "Overload candidate ignored because non-constants cannot be called "
        "with named arguments");
  }

  if (args.pos_.size() != fn_option.type()->input.size()) {
    // TODO if you call with the wrong number of arguments, even if no default
    // is available, this error occurs and that's technically a valid assessment
    // but still super misleading.
    return base::unexpected(
        "Overload candidate ignored because non-constants cannot be called "
        "with default arguments");
  }

  Binding binding(fn_option, args.pos_.size(), false);
  binding.SetPositionalArgs(args);
  binding.bound_constants_ = ctx->bound_constants_;

  DispatchTableRow dispatch_table_row(std::move(binding));

  if (!dispatch_table_row.SetTypes(fn_option, ctx)) {
    return base::unexpected("TODO");
  }
  dispatch_table_row.callable_type_ =
      &ctx->type_of(fn_option.get())->as<type::Callable>();

  return dispatch_table_row;
}

base::expected<DispatchTableRow> DispatchTableRow::Make(
    type::Typed<Expression *, type::Callable> fn_option,
    FnArgs<Expression *> const &args, Context *ctx) {
  if (fn_option.type() == nullptr) { NOT_YET(); }
  if (!IsConstant(fn_option.get())) {
    return MakeNonConstant(fn_option.as_type<type::Function>(), args, ctx);
  }

  ir::Val fn_val = backend::Evaluate(fn_option, ctx).at(0);
  if (auto *f = std::get_if<ir::AnyFunc>(&fn_val.value)) {
    return f->is_fn() ? MakeFromIrFunc(fn_option, *f->func(), args, ctx)
                      : MakeFromForeignFunction(fn_option, args, ctx);

  } else if (auto *fn = std::get_if<FunctionLiteral *>(&fn_val.value)) {
    return MakeFromFnLit(fn_option, *fn, args, ctx);

  } else {
    UNREACHABLE();
  }
}

base::expected<DispatchTableRow> DispatchTableRow::MakeFromForeignFunction(
    type::Typed<Expression *, type::Callable> fn_option,
    FnArgs<Expression *> const &args, Context *ctx) {
  // TODO while all the behavior of MakeNonConst is what we want, the name is
  // obviously incorrect. and we need to reset binding_.const_ to true. Fix
  // the name here. Probably the error messages once we have them will be
  // wrong too.
  auto result = MakeNonConstant(fn_option.as_type<type::Function>(), args, ctx);
  if (!result.has_value()) { return base::unexpected("TODO"); }
  result->binding_.const_ = true;
  result->callable_type_  = fn_option.type();
  ASSERT(result->callable_type_ != nullptr);
  return result;
}

base::expected<DispatchTableRow> DispatchTableRow::MakeFromFnLit(
    type::Typed<Expression *, type::Callable> fn_option,
    FunctionLiteral *fn_lit, FnArgs<Expression *> const &args, Context *ctx) {
  size_t binding_size =
      std::max(fn_lit->inputs.size(), args.pos_.size() + args.named_.size());

  Binding binding(fn_option, binding_size, true);
  binding.SetPositionalArgs(args);
  if (!binding.SetNamedArgs(args, fn_lit->lookup_)) {
    return base::unexpected("TODO");
  }

  Context new_ctx(ctx);
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    if (!fn_lit->inputs[i]->const_) { continue; }
    // TODO this is wrong because it needs to be removed outside the scope of
    // this function.
    new_ctx.bound_constants_.constants_.emplace(
        fn_lit->inputs[i].get(), backend::Evaluate(args.pos_[i], ctx)[0]);
  }
  for (auto & [ name, expr ] : args.named_) {
    size_t index = fn_lit->lookup_[name];
    auto *decl = fn_lit->inputs[index].get();
    if (!decl->const_) { continue; }
    new_ctx.bound_constants_.constants_.emplace(
        decl, backend::Evaluate(expr, ctx)[0]);
  }

  // TODO order these by their dependencies
  for (auto & [name, index] : fn_lit->lookup_) {
    if (index < args.pos_.size()) { continue; }
    auto iter = args.named_.find(name);
    if (iter != args.named_.end()) { continue; }
    auto *decl = fn_lit->inputs[index].get();
    decl->init_val->VerifyType(&new_ctx);
    decl->init_val->Validate(&new_ctx);
    new_ctx.bound_constants_.constants_.emplace(
        decl, backend::Evaluate(decl->init_val.get(), &new_ctx)[0]);
  }

  // TODO named arguments too.
  auto *fn_type = &ASSERT_NOT_NULL(fn_lit->VerifyTypeConcrete(&new_ctx))
                       ->as<type::Callable>();
  fn_lit->Validate(&new_ctx);
  binding.fn_.set_type(fn_type);

  DispatchTableRow dispatch_table_row(std::move(binding));
  dispatch_table_row.callable_type_ = fn_type;
  ASSERT(dispatch_table_row.callable_type_ != nullptr);
  if (!dispatch_table_row.SetTypes(*fn_lit, args, &new_ctx)) {
    return base::unexpected("TODO");
  }
  dispatch_table_row.binding_.bound_constants_ =
      std::move(new_ctx).bound_constants_;
  return dispatch_table_row;
}

base::expected<DispatchTableRow> DispatchTableRow::MakeFromIrFunc(
    type::Typed<Expression *, type::Callable> fn_option,
    ir::Func const &ir_func, FnArgs<Expression *> const &args, Context *ctx) {
  size_t binding_size =
      std::max(ir_func.args_.size(), args.pos_.size() + args.named_.size());

  Binding binding(fn_option, binding_size, true);
  binding.bound_constants_ = ctx->bound_constants_;
  binding.SetPositionalArgs(args);
  if (!binding.SetNamedArgs(args, ir_func.lookup_)) {
    return base::unexpected("TODO");
  }
  DispatchTableRow dispatch_table_row(std::move(binding));
  if (!dispatch_table_row.SetTypes(ir_func, args, ctx)) {
    return base::unexpected("TODO");
  }

  // Could be a function or a generic struct.
  dispatch_table_row.callable_type_ = fn_option.type();
  ASSERT(dispatch_table_row.callable_type_ != nullptr);
  return dispatch_table_row;
}

static const type::Type *ComputeRetType(
    base::vector<type::Callable const *> const &callable_types) {
  if (callable_types.empty()) { return nullptr; }
  std::unordered_set<size_t> sizes;
  for (auto *callable_type : callable_types) {
    if (callable_type->is<type::Function>()) {
      sizes.insert(callable_type->as<type::Function>().output.size());
    } else if (callable_type->is<type::GenericStruct>()) {
      sizes.insert(1);
    } else {
      UNREACHABLE(callable_type);
    }
  }
  if (sizes.size() != 1) {
    // TODO log an error
    return nullptr;
  }

  size_t num_outs = *sizes.begin();
  base::vector<base::vector<type::Type const *>> out_types(num_outs);
  for (size_t i = 0; i < callable_types.size(); ++i) {
    auto *callable_type = callable_types.at(i);
    ASSERT(callable_type != nullptr);
    if (callable_type->is<type::Function>()) {
      for (size_t j = 0; j < num_outs; ++j) {
        out_types[j].push_back(callable_type->as<type::Function>().output[j]);
      }
    } else if (callable_type->is<type::GenericStruct>()) {
      out_types[0].push_back(type::Type_);
    } else {
      UNREACHABLE();
    }
  }
  base::vector<type::Type const *> combined_outputs;
  combined_outputs.reserve(out_types.size());
  for (auto &ts : out_types) { combined_outputs.push_back(type::Var(std::move(ts))); }
  return type::Tup(std::move(combined_outputs));
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

  base::vector<type::Callable const *> precise_callable_types;
  for (auto &overload : overload_set) {
    // It is possible for elements of overload_set to be null. The example that
    // brought this to my attention was
    //
    // (*) ::= (lhs: Foo, rhs: int32) -> Foo { ... }
    // (*) ::= (lhs: int32, rhs: Foo) => rhs * lhs
    //
    // The intention is for the latter version to call the former as a means to
    // only implement the real logic once. But notice that in the second example
    // the type of the operator depends on knowing the type of the expression
    // `rhs * lhs`. But of course, to determine that means we need to do call
    // resolution and one of the overload set elments is the element that has
    // yet to be resolved.
    auto maybe_dispatch_table_row = DispatchTableRow::Make(overload, args, ctx);
    if (!maybe_dispatch_table_row.has_value()) {
      table.failure_reasons_.emplace(
          overload.get(), maybe_dispatch_table_row.error().to_string());
      continue;
    }

    table.total_size_ +=
        ComputeExpansion(maybe_dispatch_table_row->call_arg_types_);
    maybe_dispatch_table_row->binding_.fn_.set_type(
        maybe_dispatch_table_row->callable_type_);
    // TODO don't ned this as a field on the dispatchtablerow.
    precise_callable_types.push_back(maybe_dispatch_table_row->callable_type_);
    table.bindings_.emplace(std::move(maybe_dispatch_table_row->call_arg_types_),
                            std::move(maybe_dispatch_table_row->binding_));
  }

  // TODO this won't work with generics. Need to get the info from the table
  // itself. Probably put in in a row.
  type::Type const *ret_type = ComputeRetType(precise_callable_types);

  return std::pair{std::move(table), ret_type};
}

void Binding::SetPositionalArgs(FnArgs<Expression *> const &args) {
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
}  // namespace ast
