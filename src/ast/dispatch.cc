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

  static CallObstruction NoDefault(std::string_view arg_name) {
    return CallObstruction(NoDefaultData{std::string(arg_name)});
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
        TypeMismatchData{pos, ASSERT_NOT_NULL(bound), ASSERT_NOT_NULL(input)});
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

bool Binding::defaulted(size_t i) const {
  // TODO shouldn't need to do a linear search.
  for (auto &entry : entries_) {
    if (entry.parameter_index == i) { return entry.defaulted(); }
  }
  UNREACHABLE();
}

template <typename E>
static base::expected<Binding, CallObstruction> MakeBinding(
    type::Typed<Expression *, type::Callable> fn, FnParams<E> const &params,
    FnArgs<type::Typed<Expression *>> const &args, Context *ctx) {
  bool constant = !params.lookup_.empty();
  Binding b(fn, constant);
  base::vector<Binding::Entry> entries;

  auto p_iter = params.begin();
  auto a_iter = args.pos_.begin();

  size_t argument_index  = 0;
  size_t parameter_index = 0;
  while (p_iter != params.end() && a_iter != args.pos_.end()) {
    auto *t = a_iter->type();
    if (a_iter->get()->needs_expansion()) {
      auto const &tuple_entries = t->template as<type::Tuple>().entries_;

      // TODO check that there is enough space in params
      size_t expansion_index = 0;
      for (auto *t : tuple_entries) {
        b.entries_.emplace_back(a_iter->get(), argument_index, expansion_index,
                                parameter_index);
        ++p_iter;
        ++parameter_index;
        ++expansion_index;
      }
      ++argument_index;
      ++a_iter;
    } else {
      b.entries_.emplace_back(a_iter->get(), argument_index, -1, parameter_index);
      ++parameter_index;
      ++argument_index;
      ++p_iter;
      ++a_iter;
    }
  }

  // Fill defaults
  size_t num_pos    = b.entries_.size();
  size_t total_size = num_pos + params.lookup_.size() - parameter_index;
  b.entries_.reserve(total_size);
  while (b.entries_.size() < total_size) {
    b.entries_.emplace_back(nullptr, -1, -1, parameter_index++);
  }

  for (auto const & [ name, expr ] : args.named_) {
    if (auto iter = params.lookup_.find(name); iter != params.lookup_.end()) {
      auto &entry           = b.entries_.at(iter->second);
      entry.expr            = expr.get();
      entry.parameter_index = iter->second;
    } else {
      return CallObstruction::NoParameterNamed(name);
    }
  }

  return b;
}

namespace {
struct DispatchTableRow {
  template <typename E>
  CallObstruction SetTypes(base::vector<type::Type const *> const &input_types,
                           FnParams<E> const &params,
                           FnArgs<type::Typed<Expression *>> const &args,
                           Context *ctx);

  static base::expected<DispatchTableRow, CallObstruction> Make(
      type::Typed<Expression *, type::Callable> fn_option,
      FnArgs<type::Typed<Expression *>> const &args, Context *ctx);

  FnArgs<type::Type const *> call_arg_types_;
  type::Callable const *callable_type_ = nullptr;
  Binding binding_;

 private:
  static base::expected<DispatchTableRow, CallObstruction> MakeNonConstant(
      type::Typed<Expression *, type::Function> fn_option,
      FnArgs<type::Typed<Expression *>> const &args, Context *ctx);

  static base::expected<DispatchTableRow, CallObstruction>
  MakeFromForeignFunction(type::Typed<Expression *, type::Callable> fn_option,
                          FnArgs<type::Typed<Expression *>> const &args,
                          Context *ctx);
  static base::expected<DispatchTableRow, CallObstruction> MakeFromIrFunc(
      type::Typed<Expression *, type::Callable> fn_option,
      ir::Func const &ir_func, FnArgs<type::Typed<Expression *>> const &args,
      Context *ctx);

  static base::expected<DispatchTableRow, CallObstruction> MakeFromFnLit(
      type::Typed<Expression *, type::Callable> fn_option,
      FunctionLiteral *fn_lit, FnArgs<type::Typed<Expression *>> const &args,
      Context *ctx);
  DispatchTableRow(Binding b) : binding_(std::move(b)) {}
};

}  // namespace

template <typename E>
CallObstruction DispatchTableRow::SetTypes(
    base::vector<type::Type const *> const &input_types,
    FnParams<E> const &params, FnArgs<type::Typed<Expression *>> const &args,
    Context *ctx) {
  size_t num = 0;
  for (auto const &entry : binding_.entries_) {
    if (entry.argument_index != -1) { ++num; }
  }
  call_arg_types_.pos_.resize(num);
  for (auto & [ name, expr ] : args.named_) {
    call_arg_types_.named_.emplace(name, nullptr);
  }

  for (auto & entry : binding_.entries_) {
    if (entry.defaulted()) {
      type::Type const *input_type;
      if constexpr (std::is_same_v<E, std::unique_ptr<Declaration>>) {
        // The naming here is super confusing but if a declaration
        // "IsDefaultInitialized" that means it's of the form `foo: bar`, and so
        // it does NOT have a default value.
        Declaration &decl = *params.at(entry.parameter_index).value;
        if (decl.IsDefaultInitialized()) {
          return CallObstruction::NoDefault(decl.id_);
        }
        // TODO The order for evaluating these is wrong. Defaults may need to be
        // intermixed with non-defaults.
        auto result = decl.VerifyType(ctx);
        // TODO deal with the case where the initial value isn't a constant.
        if (!result.const_) { NOT_YET("log an error."); }
        input_type = result.type_;

        decl.Validate(ctx);
        ctx->bound_constants_.constants_.emplace(
            &decl, backend::Evaluate(decl.init_val.get(), ctx)[0]);

      } else if constexpr (std::is_same_v<E, Expression *>) {
        size_t i = entry.parameter_index;  // TODO anywhere you use this is
                                           // probably wrong and needs to be
                                           // audited.
        input_type        = input_types.at(i);
        auto const &param = params.at(entry.parameter_index);
        if (param.value == nullptr) {
          return CallObstruction::NoDefault(param.name);
        }
      } else {
        UNREACHABLE();
      }

      for (auto & e : binding_.entries_) {
        if (e.parameter_index == entry.parameter_index) {
          e.type = input_type;
        }
      }
      continue;
    }

    size_t i = entry.parameter_index;  // TODO anywhere you use this is probably
                                       // wrong and needs to be audited.
    // TODO for constant-parameters ordered processing won't work.
    // TODO variadics there may be more than one of these.
    auto *bound_type = ctx->type_of(entry.expr);
    if (entry.expansion_index != -1) {
      bound_type =
          bound_type->as<type::Tuple>().entries_.at(entry.expansion_index);
    }

    type::Type const *input_type;
    if constexpr (std::is_same_v<E, std::unique_ptr<Declaration>>) {
      input_type = ctx->type_of(params.at(entry.parameter_index).value.get());
    } else if constexpr (std::is_same_v<E, Expression *> ||
                         std::is_same_v<E, std::nullptr_t>) {
      // TODO this indexing strategy will be wrong if there are variadics, I
      // think.
      input_type = input_types.at(entry.parameter_index);
    } else {
      UNREACHABLE();
    }

    ASSIGN_OR(return CallObstruction::TypeMismatch(entry.parameter_index,
                                                   bound_type, input_type),
                     auto &match, type::Meet(bound_type, input_type));

    for (auto &e : binding_.entries_) {
      if (e.parameter_index == entry.parameter_index) { e.type = input_type; }
    }

    if (i < call_arg_types_.pos_.size()) {
      call_arg_types_.pos_.at(i) = &match;
    } else {
      auto iter = call_arg_types_.find(params.at(entry.parameter_index).name);
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
    FnArgs<type::Typed<Expression *>> const &args, Context *ctx) {
  if (!args.named_.empty()) {
    // TODO Describe `fn_option` explicitly.
    return CallObstruction::NonConstantNamedArguments();
  }

  // TODO This is not the right check. Because you could expand arguments.
  if (args.pos_.size() != fn_option.type()->input.size()) {
    // TODO if you call with the wrong number of arguments, even if no default
    // is available, this error occurs and that's technically a valid assessment
    // but still super misleading.
    return CallObstruction::NonConstantDefaults();
  }

  FnParams<std::nullptr_t> params(fn_option.type()->input.size());

  ASSIGN_OR(return _.error(), auto binding,
                   MakeBinding(fn_option, params, args, ctx));
  binding.bound_constants_ = ctx->bound_constants_;

  DispatchTableRow dispatch_table_row(std::move(binding));
  if (auto obs = dispatch_table_row.SetTypes(fn_option.type()->input, params,
                                             args, ctx);
      obs.obstructed()) {
    return obs;
  }
  dispatch_table_row.callable_type_ =
      &ctx->type_of(fn_option.get())->as<type::Callable>();

  return dispatch_table_row;
}

base::expected<DispatchTableRow, CallObstruction> DispatchTableRow::Make(
    type::Typed<Expression *, type::Callable> fn_option,
    FnArgs<type::Typed<Expression *>> const &args, Context *ctx) {
  if (fn_option.type() == nullptr) { NOT_YET(); }
  if (!IsConstant(fn_option.get())) {
    return MakeNonConstant(fn_option.as_type<type::Function>(), args, ctx);
  }

  // TODO the caller needs to ensure evaluation here is correct/safe and I
  // haven't done that yet.
  auto results = backend::Evaluate(fn_option, ctx);
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
    FnArgs<type::Typed<Expression *>> const &args, Context *ctx) {
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
    FunctionLiteral *fn_lit, FnArgs<type::Typed<Expression *>> const &args,
    Context *ctx) {
  ASSIGN_OR(return _.error(), auto binding,
                   MakeBinding(fn_option, fn_lit->inputs_, args, ctx));

  Context new_ctx(ctx);
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    // TODO wrong. this may not directly be a matchdecl but could be something
    // like an array-type of them.
    bool needs_match_decl =
        fn_lit->inputs_.at(i).value->type_expr != nullptr &&
        fn_lit->inputs_.at(i).value->type_expr->is<MatchDeclaration>();
    if (!fn_lit->inputs_.at(i).value->const_ && !needs_match_decl) { continue; }
    // TODO this is wrong because it needs to be removed outside the scope of
    // this function.
    // TODO what if this isn't evaluable? i.e., what if args.pos_[i] isn't a
    // constant. Is that a hard error or do we just ignore this case? Similarly
    // below for named and default arguments.
    if (needs_match_decl) {
      new_ctx.bound_constants_.constants_.emplace(
          &fn_lit->inputs_.at(i).value->type_expr->as<MatchDeclaration>(),
          ir::Val(args.pos_.at(i).type()));
    }

    if (fn_lit->inputs_.at(i).value->const_) {
      new_ctx.bound_constants_.constants_.emplace(
          fn_lit->inputs_.at(i).value.get(),
          backend::Evaluate(args.pos_.at(i).get(), ctx)[0]);
    }
  }
  for (auto & [ name, expr ] : args.named_) {
    size_t index = fn_lit->inputs_.lookup_[name];
    auto *decl   = fn_lit->inputs_.at(index).value.get();
    if (!decl->const_) { continue; }
    new_ctx.bound_constants_.constants_.emplace(
        decl, backend::Evaluate(expr.get(), ctx)[0]);
    // TODO Match decls?
  }

  // TODO order these by their dependencies
  for (auto & [ name, index ] : fn_lit->inputs_.lookup_) {
    if (index < args.pos_.size()) { continue; }
    auto iter = args.named_.find(std::string(name)); // TODO transparent hashing?
    if (iter != args.named_.end()) { continue; }
    auto *decl = fn_lit->inputs_.at(index).value.get();
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

  // Function literals don't need an input types vector because they have
  // constants that need to be evaluated in new_ctx anyway.
  if (auto obs =
          dispatch_table_row.SetTypes({}, fn_lit->inputs_, args, &new_ctx);
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
    ir::Func const &ir_func, FnArgs<type::Typed<Expression *>> const &args,
    Context *ctx) {
  ASSIGN_OR(return _.error(), auto binding,
                   MakeBinding(fn_option, ir_func.params_, args, ctx));
  binding.bound_constants_ = ctx->bound_constants_;

  DispatchTableRow dispatch_table_row(std::move(binding));
  if (auto obs = dispatch_table_row.SetTypes(ir_func.type_->input,
                                             ir_func.params_, args, ctx);
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
    FnArgs<type::Typed<Expression *>> const &args,
    OverloadSet const &overload_set, Context *ctx) {
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

static void AddPositionalType(type::Type const *t,
                              base::vector<FnArgs<type::Type const *>> *args) {
  if (auto *vt = t->if_as<type::Variant>()) {
    base::vector<FnArgs<type::Type const *>> new_args;
    for (auto *v : vt->variants_) {
      for (auto fnargs : *args) {
        fnargs.pos_.push_back(v);
        new_args.push_back(std::move(fnargs));
      }
    }
    *args = std::move(new_args);
  } else {
    std::for_each(
        args->begin(), args->end(),
        [t](FnArgs<type::Type const *> &fnargs) { fnargs.pos_.push_back(t); });
  }
}

static void AddNamedType(std::string const &name, type::Type const *t,
                         base::vector<FnArgs<type::Type const *>> *args) {
  if (auto *vt = t->if_as<type::Variant>()) {
    base::vector<FnArgs<type::Type const *>> new_args;
    for (auto *v : vt->variants_) {
      for (auto fnargs : *args) {
        fnargs.named_.emplace(name, v);
        new_args.push_back(std::move(fnargs));
      }
    }
    *args = std::move(new_args);
  } else {
    std::for_each(args->begin(), args->end(),
                  [&](FnArgs<type::Type const *> &fnargs) {
                    fnargs.named_.emplace(name, t);
                  });
  }
}

static base::vector<FnArgs<type::Type const *>> Expand(
    FnArgs<type::Typed<Expression *>> const &typed_args) {
  base::vector<FnArgs<type::Type const *>> all_expanded_options(1);
  for (auto const &expr : typed_args.pos_) {
    if (expr.get()->needs_expansion()) {
      for (auto *t : expr.type()->as<type::Tuple>().entries_) {
        AddPositionalType(t, &all_expanded_options);
      }
    } else {
      AddPositionalType(expr.type(), &all_expanded_options);
    }
  }
  for (auto const & [ name, expr ] : typed_args.named_) {
    if (expr.get()->needs_expansion()) {
      for (auto *t : expr.type()->as<type::Tuple>().entries_) {
        AddNamedType(name, t, &all_expanded_options);
      }
    } else {
      AddNamedType(name, expr.type(), &all_expanded_options);
    }
  }
  return all_expanded_options;
}

type::Type const *DispatchTable::MakeOrLogError(
    Node *node, FnArgs<Expression *> const &args,
    OverloadSet const &overload_set, Context *ctx, bool repeated) {

  // TODO pull this out one more layer into the VerifyType call of node.
  auto typed_args = args.Transform([ctx](Expression *expr) {
    return type::Typed<Expression *>(expr, ASSERT_NOT_NULL(ctx->type_of(expr)));
  });

  auto[table, ret_type] = Make(typed_args, overload_set, ctx);
  if (table.bindings_.empty()) {
    // TODO what about operators?
    ctx->error_log_.NoCallMatch(node->span, table.failure_reasons_);
    return nullptr;
  }

  auto expanded = Expand(typed_args);
  auto new_end_iter = std::remove_if(
      expanded.begin(), expanded.end(),
      [&](FnArgs<type::Type const *> const &fnargs) {
        return std::any_of(
            table.bindings_.begin(), table.bindings_.end(),
            [&fnargs](auto const &kv) { return Covers(kv.first, fnargs); });
      });
  expanded.erase(new_end_iter, expanded.end());
  if (!expanded.empty()) {
    ctx->error_log_.MissingDispatchContingency(node->span, expanded);
    return nullptr;
  }

  if (repeated) {
    ctx->push_rep_dispatch_table(node, std::move(table));
    return ret_type;
  } else {
    ctx->set_dispatch_table(&node->as<Expression>(), std::move(table));
    return ctx->set_type(&node->as<Expression>(), ret_type);
  }
}

// We allow overwriting outgoing_regs slots. This will only happen with locally
// declared registers which means they're all simple and this works as a nice
// return value.
static void EmitOneCallDispatch(
    type::Type const *ret_type, base::vector<ir::Val> *outgoing_regs,
    base::unordered_map<Expression *, base::vector<ir::Val> const *> const
        &expr_map,
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
  FnParams<Expression *> *const_params = nullptr;
  if (auto *fn_to_call = std::get_if<ir::AnyFunc>(&callee.value)) {
    if (fn_to_call->is_fn()) { const_params = &(fn_to_call->func()->params_); }
  }

  base::vector<ir::Val> args;
  args.resize(binding.entries_.size());
  for (size_t i = 0; i < args.size(); ++i) {
    auto entry = binding.entries_.at(i);
    if (entry.defaulted()) {
      Expression *default_expr = (*ASSERT_NOT_NULL(const_params)).at(i).value;
      args[i]                  = ASSERT_NOT_NULL(entry.type)
                    ->PrepareArgument(ctx->type_of(default_expr),
                                      default_expr->EmitIR(ctx)[0], ctx);
    } else {
      auto *t = (entry.expansion_index == -1)
                    ? ctx->type_of(entry.expr)
                    : ctx->type_of(entry.expr)
                          ->as<type::Tuple>()
                          .entries_.at(entry.expansion_index);
      auto const &val =
          expr_map.at(entry.expr)->at(std::max(0, entry.expansion_index));
      args[i] = ASSERT_NOT_NULL(entry.type)->PrepareArgument(t, val, ctx);
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

// Small contains expanded arguments (no variants).
bool Covers(FnArgs<type::Type const *> const &big,
            FnArgs<type::Type const *> const &small) {
  ASSERT(big.pos_.size() == small.pos_.size());
  for (size_t i = 0; i < big.pos_.size(); ++i) {
    if (big.pos_.at(i) == small.pos_.at(i)) { continue; }
    if (auto *vt = big.pos_.at(i)->if_as<type::Variant>()) {
      if (vt->contains(small.pos_.at(i))) { continue; }
    }
    return false;
  }
  for (auto const & [ name, t ] : small.named_) {
    auto iter = big.named_.find(name);
    if (iter == big.named_.end()) { return false; }
    if (t == iter->second) { continue; }
    if (auto *vt = iter->second->if_as<type::Variant>()) {
      if (vt->contains(t)) { continue; }
    }
    return false;
  }
  return true;
}

static ir::BlockIndex CallLookupTest(
    FnArgs<std::pair<Expression *, base::vector<ir::Val>>> const &args,
    FnArgs<type::Type const *> const &call_arg_type, Context *ctx) {
  // Generate code that attempts to match the types on each argument (only
  // check the ones at the call-site that could be variants).
  
  // TODO enable variant dispatch on arguments that got expanded.
  auto next_binding = ir::Func::Current->AddBlock();
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    if (!ctx->type_of(args.pos_[i].first)->is<type::Variant>()) { continue; }
    ir::BasicBlock::Current = ir::EarlyExitOn<false>(
        next_binding, EmitVariantMatch(std::get<ir::Register>(
                                           args.pos_.at(i).second[0].value),
                                       call_arg_type.pos_[i]));
  }

  for (const auto & [ name, expr_and_val ] : args.named_) {
    auto iter = call_arg_type.find(name);
    if (iter == call_arg_type.named_.end()) { continue; }
    if (!ctx->type_of(expr_and_val.first)->is<type::Variant>()) { continue; }
    ir::BasicBlock::Current = ir::EarlyExitOn<false>(
        next_binding,
        EmitVariantMatch(
            std::get<ir::Register>(args.named_.at(iter->first).second[0].value),
            iter->second));
  }

  return next_binding;
}

base::vector<ir::Val> DispatchTable::EmitCall(
    FnArgs<std::pair<Expression *, base::vector<ir::Val>>> const &args,
    type::Type const *ret_type, Context *ctx) const {
  ASSERT(bindings_.size() != 0u);
  base::unordered_map<Expression *, base::vector<ir::Val> const *> expr_map;
  args.Apply(
      [&expr_map](std::pair<Expression *, base::vector<ir::Val>> const &arg) {
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
