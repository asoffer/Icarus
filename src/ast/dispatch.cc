#include "ast/dispatch.h"

#include <variant>

#include "ast/call.h"
#include "ast/function_literal.h"
#include "ast/match_declaration.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "context.h"
#include "ir/cmd.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "module.h"
#include "scope.h"
#include "type/cast.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/pointer.h"
#include "type/tuple.h"
#include "type/variant.h"

extern i32 ForeignFuncIndex;

namespace {
// Reason why the particular function could not be called.
struct CallObstruction {
  static CallObstruction None() { return CallObstruction(NoneData{}); }

  static CallObstruction NoParameterNamed(std::string name) {
    return CallObstruction(NoParameterNamedData{std::move(name)});
  }

  static CallObstruction NoDefault(std::string arg_name) {
    return CallObstruction(NoDefaultData{std::move(arg_name)});
  }

  static CallObstruction NonConstantNamedArguments() {
    return CallObstruction(NonConstantNamedArgumentsData{});
  }

  static CallObstruction NonConstantDefaults() {
    return CallObstruction(NonConstantDefaultsData{});
  }

  static CallObstruction TypeMismatch(size_t pos, type::Type const *bound,
                                      type::Type const *input) {
    return CallObstruction(
        TypeMismatchData{pos, std::move(bound), std::move(input)});
  }

  static CallObstruction CascadingError() {
    return CallObstruction(CascadingErrorData{});
  }

  constexpr bool obstructed() const {
    return !std::holds_alternative<NoneData>(data_);
  }
  std::string to_string() const {
    return std::visit(
        base::overloaded{
            [](NoneData) -> std::string { return ""; },
            [](NoParameterNamedData const &d) -> std::string {
              return "Function has no argument named `" + d.name_ + "'";
            },
            [](NoDefaultData const &d) -> std::string {
              return "Overload ignored because no value provided for argument "
                     "`" +
                     d.name_ + "', which has no default.";
            },
            [](TypeMismatchData const &d) -> std::string {
              // TODO clarify that this is zero-indexed?
              return "Overload candidate ignored because parameter " +
                     std::to_string(d.position_) + " has type " +
                     d.input_->to_string() +
                     " which does not match what you provided (" +
                     d.bound_->to_string() + ")";
            },
            [](NonConstantNamedArgumentsData) -> std::string {
              return "Overload candidate ignored because non-constants cannot "
                     "be called with named arguments";
            },
            [](NonConstantDefaultsData) -> std::string {
              return "Overload candidate ignored because non-constants cannot "
                     "be called with default arguments";
            },
            [](CascadingErrorData) -> std::string { UNREACHABLE(); }},
        data_);
  }

  template <typename T> bool is() const {
    return std::holds_alternative<T>(data_);
  }

  struct NoneData {};
  struct NoParameterNamedData {
    std::string name_;
  };
  struct TypeMismatchData {
    size_t position_;
    type::Type const *bound_;
    type::Type const *input_;
  };
  struct NoDefaultData {
    std::string name_;
  };
  struct NonConstantNamedArgumentsData {};
  struct NonConstantDefaultsData {};
  struct CascadingErrorData {};

 private:
  template <typename T>
  CallObstruction(T &&data) : data_(std::forward<T>(data)) {}

  std::variant<NoneData, NoParameterNamedData, TypeMismatchData, NoDefaultData,
               NonConstantNamedArgumentsData, NonConstantDefaultsData,
               CascadingErrorData>
      data_;
};
}  // namespace

namespace ast {
using base::check::Is;

base::expected<Binding, CallObstruction> MakeBinding(
    type::Typed<Expression *, type::Callable> fn,
    FnArgs<Expression *> const &args, size_t n,
    base::unordered_map<std::string, size_t> const *index_lookup = nullptr) {
  bool constant = (index_lookup != nullptr);
  Binding b(fn, n, constant);
  if (constant) {
    b.SetPositionalArgs(args);
    for (auto const & [ name, expr ] : args.named_) {
      if (auto iter = index_lookup->find(name); iter != index_lookup->end()) {
        b.exprs_.at(iter->second) = type::Typed<Expression *>(expr, nullptr);
      } else {
        return CallObstruction::NoParameterNamed(name);
      }
    }
  } else {
    b.SetPositionalArgs(args);
  }
  return b;
}

void Binding::SetPositionalArgs(FnArgs<Expression *> const &args) {
  ASSERT(exprs_.size() >= args.pos_.size());
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    exprs_[i] = type::Typed<Expression *>(args.pos_.at(i), nullptr);
  }
}

namespace {
struct DispatchTableRow {
  CallObstruction SetTypes(type::Typed<Expression *, type::Function> fn,
                           Context *ctx);
  CallObstruction SetTypes(ir::Func const &fn, FnArgs<Expression *> const &args,
                           Context *ctx);
  CallObstruction SetTypes(FunctionLiteral const &fn,
                           FnArgs<Expression *> const &args, Context *ctx);
  static base::expected<DispatchTableRow, CallObstruction> Make(
      type::Typed<Expression *, type::Callable> fn_option,
      FnArgs<Expression *> const &args, Context *ctx);

  FnArgs<type::Type const *> call_arg_types_;
  type::Callable const *callable_type_ = nullptr;
  Binding binding_;

 private:
  static base::expected<DispatchTableRow, CallObstruction> MakeNonConstant(
      type::Typed<Expression *, type::Function> fn_option,
      FnArgs<Expression *> const &args, Context *ctx);

  static base::expected<DispatchTableRow, CallObstruction>
  MakeFromForeignFunction(type::Typed<Expression *, type::Callable> fn_option,
                          FnArgs<Expression *> const &args, Context *ctx);
  static base::expected<DispatchTableRow, CallObstruction> MakeFromIrFunc(
      type::Typed<Expression *, type::Callable> fn_option,
      ir::Func const &ir_func, FnArgs<Expression *> const &args, Context *ctx);

  static base::expected<DispatchTableRow, CallObstruction> MakeFromFnLit(
      type::Typed<Expression *, type::Callable> fn_option,
      FunctionLiteral *fn_lit, FnArgs<Expression *> const &args, Context *ctx);
  DispatchTableRow(Binding b) : binding_(std::move(b)) {}
};

}  // namespace

CallObstruction DispatchTableRow::SetTypes(
    type::Typed<Expression *, type::Function> fn, Context *ctx) {
  ASSERT(fn.type()->input.size() == binding_.exprs_.size());
  call_arg_types_.pos_.resize(fn.type()->input.size());
  auto const &input_types = fn.type()->input;
  for (size_t i = 0; i < binding_.exprs_.size(); ++i) {
    auto *bound_type = ctx->type_of(binding_.exprs_.at(i).get());
    auto *input_type = input_types.at(i);
    ASSIGN_OR(return CallObstruction::TypeMismatch(i, bound_type, input_type),
                     auto &match, type::Meet(bound_type, input_type));

    binding_.exprs_.at(i).set_type(input_types.at(i));
    call_arg_types_.pos_.at(i) = &match;
  }

  return CallObstruction::None();
}

CallObstruction DispatchTableRow::SetTypes(FunctionLiteral const &fn,
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
      auto &decl = *fn.inputs.at(i);
      if (decl.IsDefaultInitialized()) {
        return CallObstruction::NoDefault(decl.id_);
      }
      // TODO The order for evaluating these is wrong. Defaults may need to be
      // intermixed with non-defaults.
      auto result = fn.inputs.at(i).get()->VerifyType(ctx);
      // TODO deal with the case where the initial value isn't a constant.
      if (!result.const_) { NOT_YET("log an error."); }

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

  return CallObstruction::None();
}

CallObstruction DispatchTableRow::SetTypes(ir::Func const &fn,
                                           FnArgs<Expression *> const &args,
                                           Context *ctx) {
  call_arg_types_.pos_.resize(args.pos_.size());
  for (auto & [ name, expr ] : args.named_) {
    call_arg_types_.named_.emplace(name, nullptr);
  }

  auto const &input_types = fn.type_->input;
  for (size_t i = 0; i < binding_.exprs_.size(); ++i) {
    if (binding_.defaulted(i)) {
      if (fn.args_.at(i).second == nullptr) {
        return CallObstruction::NoDefault(fn.args_.at(i).first);
      }
      binding_.exprs_.at(i).set_type(input_types.at(i));
      continue;
    }

    auto *bound_type = ctx->type_of(binding_.exprs_.at(i).get());
    auto *input_type = input_types.at(i);
    ASSIGN_OR(return CallObstruction::TypeMismatch(i, bound_type, input_type),
                     auto &match, type::Meet(bound_type, input_type));

    binding_.exprs_.at(i).set_type(input_types.at(i));

    if (i < call_arg_types_.pos_.size()) {
      call_arg_types_.pos_.at(i) = &match;
    } else {
      auto iter = call_arg_types_.find(fn.args_.at(i).first);
      ASSERT(iter != call_arg_types_.named_.end());
      iter->second = &match;
    }
  }
  return CallObstruction::None();
}

static bool IsConstant(Expression *e) {
  if (e->is<Call>()) {
    if (auto *c = &e->as<Call>(); c->fn_->is<Terminal>()) {
      ASSIGN_OR(return false, auto &bgi,
                       std::get_if<ir::BuiltinGenericIndex>(
                           &c->fn_->as<Terminal>().value.value));
      return bgi == ir::BuiltinGenericIndex{ForeignFuncIndex};
    }
  }
  return  e->is<FunctionLiteral>() ||
         (e->is<Declaration>() && e->as<Declaration>().const_);
}

base::expected<DispatchTableRow, CallObstruction> DispatchTableRow::MakeNonConstant(
    type::Typed<Expression *, type::Function> fn_option,
    FnArgs<Expression *> const &args, Context *ctx) {
  if (!args.named_.empty()) {
    // TODO Describe `fn_option` explicitly.
    return CallObstruction::NonConstantNamedArguments();
  }

  if (args.pos_.size() != fn_option.type()->input.size()) {
    // TODO if you call with the wrong number of arguments, even if no default
    // is available, this error occurs and that's technically a valid assessment
    // but still super misleading.
    return CallObstruction::NonConstantDefaults();
  }

  ASSIGN_OR(return _.error(), auto binding,
                   MakeBinding(fn_option, args, args.pos_.size()));
  binding.bound_constants_ = ctx->bound_constants_;

  DispatchTableRow dispatch_table_row(std::move(binding));

  if (auto obs = dispatch_table_row.SetTypes(fn_option, ctx);
      obs.obstructed()) {
    return obs;
  }
  dispatch_table_row.callable_type_ =
      &ctx->type_of(fn_option.get())->as<type::Callable>();

  return dispatch_table_row;
}

base::expected<DispatchTableRow, CallObstruction> DispatchTableRow::Make(
    type::Typed<Expression *, type::Callable> fn_option,
    FnArgs<Expression *> const &args, Context *ctx) {
  if (fn_option.type() == nullptr) { NOT_YET(); }
  if (!IsConstant(fn_option.get())) {
    return MakeNonConstant(fn_option.as_type<type::Function>(), args, ctx);
  }

  // TODO the caller needs to ensure evaluation here is correct/safe and I
  // haven't done that yet.
  auto results   = backend::Evaluate(fn_option, ctx);
  if (results.empty()) {  // Meaning there was an error in ctx earlier
    return CallObstruction::CascadingError();
  }

  ir::Val fn_val = results.at(0);

  if (auto *f = std::get_if<ir::AnyFunc>(&fn_val.value)) {
    return f->is_fn() ? MakeFromIrFunc(fn_option, *f->func(), args, ctx)
                      : MakeFromForeignFunction(fn_option, args, ctx);

  } else if (auto *fn = std::get_if<FunctionLiteral *>(&fn_val.value)) {
    return MakeFromFnLit(fn_option, *fn, args, ctx);

  } else {
    UNREACHABLE();
  }
}

base::expected<DispatchTableRow, CallObstruction>
DispatchTableRow::MakeFromForeignFunction(
    type::Typed<Expression *, type::Callable> fn_option,
    FnArgs<Expression *> const &args, Context *ctx) {
  // TODO while all the behavior of MakeNonConst is what we want, the name is
  // obviously incorrect. and we need to reset binding_.const_ to true. Fix
  // the name here. Probably the error messages once we have them will be
  // wrong too.
  ASSIGN_OR(
      return _.error(), auto result,
             MakeNonConstant(fn_option.as_type<type::Function>(), args, ctx));
  result.binding_.const_ = true;
  result.callable_type_  = fn_option.type();
  ASSERT(result.callable_type_ != nullptr);
  return result;
}

base::expected<DispatchTableRow, CallObstruction>
DispatchTableRow::MakeFromFnLit(
    type::Typed<Expression *, type::Callable> fn_option,
    FunctionLiteral *fn_lit, FnArgs<Expression *> const &args, Context *ctx) {
  size_t num =
      std::max(fn_lit->inputs.size(), args.pos_.size() + args.named_.size());
  ASSIGN_OR(return _.error(), auto binding,
                   MakeBinding(fn_option, args, num, &fn_lit->lookup_));

  Context new_ctx(ctx);
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    if (!fn_lit->inputs[i]->const_) { continue; }
    // TODO this is wrong because it needs to be removed outside the scope of
    // this function.
    // TODO what if this isn't evaluable? i.e., what if args.pos_[i] isn't a
    // constant. Is that a hard error or do we just ignore this case? Similarly
    // below for named and default arguments.
    new_ctx.bound_constants_.constants_.emplace(
        fn_lit->inputs[i].get(), backend::Evaluate(args.pos_[i], ctx)[0]);
  }
  for (auto & [ name, expr ] : args.named_) {
    size_t index = fn_lit->lookup_[name];
    auto *decl   = fn_lit->inputs[index].get();
    if (!decl->const_) { continue; }
    new_ctx.bound_constants_.constants_.emplace(
        decl, backend::Evaluate(expr, ctx)[0]);
  }

  // TODO order these by their dependencies
  for (auto & [ name, index ] : fn_lit->lookup_) {
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
  auto *fn_type = &ASSERT_NOT_NULL(fn_lit->VerifyTypeConcrete(&new_ctx).type_)
                       ->as<type::Callable>();
  fn_lit->Validate(&new_ctx);
  binding.fn_.set_type(fn_type);

  DispatchTableRow dispatch_table_row(std::move(binding));
  dispatch_table_row.callable_type_ = fn_type;
  ASSERT(dispatch_table_row.callable_type_ != nullptr);
  if (auto obs = dispatch_table_row.SetTypes(*fn_lit, args, &new_ctx);
      obs.obstructed()) {
    return obs;
  }
  dispatch_table_row.binding_.bound_constants_ =
      std::move(new_ctx).bound_constants_;
  return dispatch_table_row;
}

base::expected<DispatchTableRow, CallObstruction>
DispatchTableRow::MakeFromIrFunc(
    type::Typed<Expression *, type::Callable> fn_option,
    ir::Func const &ir_func, FnArgs<Expression *> const &args, Context *ctx) {
  size_t num =
      std::max(ir_func.args_.size(), args.pos_.size() + args.named_.size());
  ASSIGN_OR(return _.error(), auto binding,
                   MakeBinding(fn_option, args, num, &ir_func.lookup_));
  binding.bound_constants_ = ctx->bound_constants_;

  DispatchTableRow dispatch_table_row(std::move(binding));
  if (auto obs = dispatch_table_row.SetTypes(ir_func, args, ctx);
      obs.obstructed()) {
    return obs;
  }

  // Could be a function or a generic struct.
  dispatch_table_row.callable_type_ = fn_option.type();
  ASSERT(dispatch_table_row.callable_type_ != nullptr);
  return dispatch_table_row;
}

static type::Type const *ComputeRetType(
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
  for (auto &ts : out_types) {
    combined_outputs.push_back(type::Var(std::move(ts)));
  }
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
      if (maybe_dispatch_table_row.error()
              .is<CallObstruction::CascadingErrorData>()) {
        // TODO return from this function by some mechanism indicating that we
        // gave up because there were errors resolving the call.
        return {};
      }
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
    table.bindings_.emplace(
        std::move(maybe_dispatch_table_row->call_arg_types_),
        std::move(maybe_dispatch_table_row->binding_));
  }

  // TODO this won't work with generics. Need to get the info from the table
  // itself. Probably put in in a row.
  type::Type const *ret_type = ComputeRetType(precise_callable_types);

  return std::pair{std::move(table), ret_type};
}

// We allow overwriting outgoing_regs slots. This will only happen with locally
// declared registers which means they're all simple and this works as a nice
// return value.
static void EmitOneCallDispatch(
    type::Type const *ret_type, base::vector<ir::Val> *outgoing_regs,
    base::unordered_map<Expression *, const ir::Val *> const &expr_map,
    Binding const &binding, Context *ctx) {
  auto callee = [&] {
    Context fn_ctx(ctx->mod_);  // TODO this might be the wrong module.
    fn_ctx.bound_constants_ = binding.bound_constants_;
    return binding.fn_.get()->EmitIR(&fn_ctx)[0];
  }();

  if (!binding.const_) {
    if (!binding.fn_.get()->is<Declaration>() ||
        !binding.fn_.get()->as<Declaration>().is_fn_param_) {
      if (auto *reg = std::get_if<ir::Register>(&callee.value)) {
        // TODO this feels like a hack, there should be a better way to
        // determine if the function
        callee = ir::Val::Reg(ir::Load<ir::AnyFunc>(*reg, binding.fn_.type()),
                              binding.fn_.type());
      }
    }
  }
  ASSERT(callee.type, Is<type::Callable>());

  // After the last check, if you pass, you should dispatch
  base::vector<std::pair<std::string, Expression *>> *const_args = nullptr;
  if (auto *fn_to_call = std::get_if<ir::AnyFunc>(&callee.value)) {
    if (fn_to_call->is_fn()) { const_args = &(fn_to_call->func()->args_); }
  }

  base::vector<ir::Val> args;
  args.resize(binding.exprs_.size());
  for (size_t i = 0; i < args.size(); ++i) {
    auto typed_expr = binding.exprs_[i];
    if (typed_expr.get() == nullptr) {
      auto default_expr = (*ASSERT_NOT_NULL(const_args))[i].second;
      args[i]           = ASSERT_NOT_NULL(typed_expr.type())
                    ->PrepareArgument(ctx->type_of(default_expr),
                                      default_expr->EmitIR(ctx)[0], ctx);
    } else {
      args[i] = ASSERT_NOT_NULL(typed_expr.type())
                    ->PrepareArgument(ctx->type_of(typed_expr.get()),
                                      *expr_map.at(typed_expr.get()), ctx);
    }
  }

  ir::Arguments call_args;
  call_args.type_ = &callee.type->as<type::Callable>();
  for (const auto &arg : args) { call_args.append(arg); }

  base::vector<ir::Val> results;
  ir::OutParams outs;

  // TODO don't copy the vector.
  base::vector<type::Type const *> out_types;
  if (binding.fn_.type()->is<type::Function>()) {
    out_types = binding.fn_.type()->as<type::Function>().output;
  } else if (binding.fn_.type()->is<type::GenericStruct>()) {
    out_types.push_back(type::Type_);
  } else {
    UNREACHABLE();
  }

  if (!out_types.empty()) {
    auto MakeRegister = [&](type::Type const *return_type,
                            type::Type const *expected_return_type,
                            ir::Val *out_reg) {
      // Cases:
      // 1. I return a small value, and am expected to return the same
      //    reg return
      //
      // 2. I return a big value and am expected to return the same
      //    pass in a return
      // 3. I return a variant and am expected to return a variant
      //    pass in a return
      //
      // 4. I return a small value but am expected to return a variant
      //    pass in a return and fix
      // 5. I return a big value but am expected to return a variant
      //    pass in a return and fix
      //
      // TODO: This is a lot like PrepareArgument.
      if (!return_type->is_big() && !expected_return_type->is_big()) {
        *out_reg = ir::Val::Reg(outs.AppendReg(expected_return_type),
                                expected_return_type);
        return;
      }

      if (return_type == expected_return_type ||
          return_type->is<type::Variant>()) {
        outs.AppendLoc(std::get<ir::Register>(out_reg->value));
        return;
      }

      ASSERT(expected_return_type, Is<type::Variant>());
      ir::Store(return_type,
                ir::VariantType(std::get<ir::Register>(out_reg->value)));
      outs.AppendLoc(ir::VariantValue(return_type,
                                      std::get<ir::Register>(out_reg->value)));
    };

    if (ret_type->is<type::Tuple>()) {
      ASSERT(ret_type->as<type::Tuple>().entries_.size() == out_types.size());
      for (size_t i = 0; i < out_types.size(); ++i) {
        MakeRegister(out_types.at(i),
                     ret_type->as<type::Tuple>().entries_.at(i),
                     &outgoing_regs->at(i));
      }
    } else {
      MakeRegister(out_types.at(0), ret_type, &outgoing_regs->at(0));
    }
  }

  ASSERT(std::holds_alternative<ir::Register>(callee.value) ||
         std::holds_alternative<ir::AnyFunc>(callee.value));
  ir::Call(callee.reg_or<ir::AnyFunc>(), std::move(call_args), std::move(outs));
}

static ir::RegisterOr<bool> EmitVariantMatch(ir::Register needle,
                                             type::Type const *haystack) {
  auto runtime_type = ir::Load<type::Type const *>(ir::VariantType(needle));

  if (haystack->is<type::Variant>()) {
    // TODO I'm fairly confident this will work, but it's also overkill because
    // we may already know this type matches if one variant is a subset of the
    // other.
    auto landing = ir::Func::Current->AddBlock();

    base::unordered_map<ir::BlockIndex, ir::RegisterOr<bool>> phi_map;
    for (type::Type const *v : haystack->as<type::Variant>().variants_) {
      phi_map.emplace(ir::BasicBlock::Current, true);

      ir::BasicBlock::Current =
          ir::EarlyExitOn<true>(landing, ir::Eq(v, runtime_type));
    }

    phi_map.emplace(ir::BasicBlock::Current, false);

    ir::UncondJump(landing);

    ir::BasicBlock::Current = landing;
    return ir::MakePhi<bool>(ir::Phi(type::Bool), phi_map);

  } else {
    // TODO actually just implicitly convertible to haystack
    return ir::Eq(haystack, runtime_type);
  }
}

static ir::BlockIndex CallLookupTest(
    FnArgs<std::pair<Expression *, ir::Val>> const &args,
    FnArgs<type::Type const *> const &call_arg_type, Context *ctx) {
  // Generate code that attempts to match the types on each argument (only
  // check the ones at the call-site that could be variants).
  auto next_binding = ir::Func::Current->AddBlock();
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    if (!ctx->type_of(args.pos_[i].first)->is<type::Variant>()) { continue; }
    ir::BasicBlock::Current = ir::EarlyExitOn<false>(
        next_binding,
        EmitVariantMatch(std::get<ir::Register>(args.pos_.at(i).second.value),
                         call_arg_type.pos_[i]));
  }

  for (const auto & [ name, expr_and_val ] : args.named_) {
    auto iter = call_arg_type.find(name);
    if (iter == call_arg_type.named_.end()) { continue; }
    if (!ctx->type_of(expr_and_val.first)->is<type::Variant>()) { continue; }
    ir::BasicBlock::Current = ir::EarlyExitOn<false>(
        next_binding,
        EmitVariantMatch(
            std::get<ir::Register>(args.named_.at(iter->first).second.value),
            iter->second));
  }

  return next_binding;
}

base::vector<ir::Val> DispatchTable::EmitCall(
    FnArgs<std::pair<Expression *, ir::Val>> const &args,
    type::Type const *ret_type, Context *ctx) const {
  ASSERT(bindings_.size() != 0u);
  base::unordered_map<Expression *, const ir::Val *> expr_map;
  args.Apply([&expr_map](const std::pair<Expression *, ir::Val> &arg) {
    expr_map[arg.first] = &arg.second;
  });

  base::vector<ir::Val> out_regs;
  if (ret_type->is<type::Tuple>()) {
    out_regs.reserve(ret_type->as<type::Tuple>().entries_.size());
    for (auto *entry : ret_type->as<type::Tuple>().entries_) {
      out_regs.push_back(entry->is_big()
                             ? ir::Val::Reg(ir::Alloca(entry), type::Ptr(entry))
                             : ir::Val::None());
    }
  } else {
    out_regs.push_back(ret_type->is_big() ? ir::Val::Reg(ir::Alloca(ret_type),
                                                         type::Ptr(ret_type))
                                          : ir::Val::None());
  }

  if (bindings_.size() == 1) {
    const auto & [ call_arg_type, binding ] = *bindings_.begin();
    EmitOneCallDispatch(ret_type, &out_regs, expr_map, binding, ctx);
    return out_regs;
  }

  // TODO push void out of here.
  size_t num_rets = ret_type->is<type::Tuple>()
                        ? ret_type->as<type::Tuple>().entries_.size()
                        : 1;

  base::vector<base::unordered_map<ir::BlockIndex, ir::Val>> result_phi_args(
      num_rets);

  auto landing_block = ir::Func::Current->AddBlock();

  auto iter = bindings_.begin();
  ASSERT(iter != bindings_.end());
  for (size_t i = 0; i < bindings_.size() - 1; ++i, ++iter) {
    const auto & [ call_arg_type, binding ] = *iter;
    auto next_binding = CallLookupTest(args, call_arg_type, ctx);
    size_t j          = 0;

    EmitOneCallDispatch(ret_type, &out_regs, expr_map, binding, ctx);
    for (const auto &result : out_regs) {
      result_phi_args.at(j)[ir::BasicBlock::Current] = result;
      ++j;
    }
    ASSERT(j == num_rets);

    ir::UncondJump(landing_block);
    ir::BasicBlock::Current = next_binding;
  }

  const auto & [ call_arg_type, binding ] = *iter;
  size_t j                                = 0;
  EmitOneCallDispatch(ret_type, &out_regs, expr_map, binding, ctx);
  for (const auto &result : out_regs) {
    result_phi_args.at(j)[ir::BasicBlock::Current] = result;
    ++j;
  }
  ASSERT(j == num_rets);

  ir::UncondJump(landing_block);
  ir::BasicBlock::Current = landing_block;

  switch (num_rets) {
    case 0: return {};
    case 1:
      if (ret_type == type::Void()) {
        return {ir::Val::None()};
      } else {
        return {
            ir::MakePhi(ir::Phi(ret_type->is_big() ? Ptr(ret_type) : ret_type),
                        result_phi_args[0])};
      }
      break;
    default: {
      base::vector<ir::Val> results;
      results.reserve(num_rets);
      const auto &tup_entries = ret_type->as<type::Tuple>().entries_;
      for (size_t i = 0; i < num_rets; ++i) {
        const type::Type *single_ret_type = tup_entries[i];
        if (single_ret_type == type::Void()) {
          results.push_back(ir::Val::None());
        } else {
          results.push_back(ir::MakePhi(
              ir::Phi(single_ret_type->is_big() ? Ptr(single_ret_type)
                                                : single_ret_type),
              result_phi_args[i]));
        }
      }
      return results;
    } break;
  }
  UNREACHABLE();
}

}  // namespace ast
