#include "ast/dispatch/table.h"

#include <variant>

#include "ast/block_literal.h"
#include "ast/builtin_fn.h"
#include "ast/call.h"
#include "ast/function_literal.h"
#include "ast/match_declaration.h"
#include "backend/eval.h"
#include "ir/cmd.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "misc/context.h"
#include "misc/module.h"
#include "misc/scope.h"
#include "type/cast.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/pointer.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace {
}  // namespace

namespace ast {
using ::matcher::InheritsFrom;

template <typename E>
static base::expected<Binding, CallObstruction> MakeBinding(
    type::Typed<Expression *, type::Callable> fn, FnParams<E> const &params,
    FnArgs<type::Typed<Expression *>> const &args, Context *ctx) {
  bool constant = !params.lookup_.empty();
  Binding b(fn, constant);

  ASSIGN_OR(return _.error(), b.arg_res_, ArgResolution::Make(params, args));
  return b;
}

namespace {
struct DispatchTableRow {
  template <typename E>
  CallObstruction SetTypes(std::vector<type::Type const *> const &input_types,
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
  MakeFromBlockSequence(ir::BlockSequence bs,
                        FnArgs<type::Typed<Expression *>> const &args,
                        Context *ctx);

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
    std::vector<type::Type const *> const &input_types,
    FnParams<E> const &params, FnArgs<type::Typed<Expression *>> const &args,
    Context *ctx) {
  std::unordered_map<std::string, type::Type const *> named;
  for (auto &[name, expr] : args.named()) { named.emplace(name, nullptr); }

  size_t num_pos  = binding_.arg_res_.num_positional_arguments();
  call_arg_types_ = FnArgs(std::vector<type::Type const *>(num_pos, nullptr),
                           std::move(named));
  return binding_.arg_res_.SetTypes(input_types, params, ctx, &call_arg_types_);
}

static bool IsConstant(Expression *e) {
  return e->is<FunctionLiteral>() || e->is<BuiltinFn>() ||
         (e->is<Declaration>() && e->as<Declaration>().const_);
}

base::expected<DispatchTableRow, CallObstruction>
DispatchTableRow::MakeNonConstant(
    type::Typed<Expression *, type::Function> fn_option,
    FnArgs<type::Typed<Expression *>> const &args, Context *ctx) {
  if (args.num_named() != 0u) {
    // TODO Describe `fn_option` explicitly.
    return CallObstruction::NonConstantNamedArguments();
  }

  // TODO This is not the right check. Because you could expand arguments.
  if (args.num_pos() != fn_option.type()->input.size()) {
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
  if (fn_option.type() == nullptr) { NOT_YET(fn_option.get()->to_string(0)); }
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

  } else if (auto *fn = std::get_if<ir::BlockSequence>(&fn_val.value)) {
    return MakeFromBlockSequence(*fn, args, ctx);
  } else {
    UNREACHABLE();
  }
}

base::expected<DispatchTableRow, CallObstruction>
DispatchTableRow::MakeFromBlockSequence(
    ir::BlockSequence bs, FnArgs<type::Typed<Expression *>> const &args,
    Context *ctx) {
  // TODO figure out which one to call. For now just calling the last entry.

  // ir::Call(callee, std::move(call_args), std::move(outs));
  // TODO pick the right one to call.
  // TODO

  for (auto *block_lit : *bs.seq_) {
    if (block_lit == nullptr) {
      base::Log() << "start";
    } else if (block_lit == reinterpret_cast<BlockLiteral *>(0x01)) {
      base::Log() << "exit";
    } else {
      for (auto const& e : block_lit->before_) {
        base::Log() << e.to_string(0);
      }
    }
  }

  FnParams<std::nullptr_t> params(1);

  ASSIGN_OR(return _.error(), auto binding,
                   MakeBinding(
                       type::Typed<Expression *, type::Callable>{
                           &bs.seq_->back()->before_.at(0), type::Blk()},
                       params, args, ctx));
   DispatchTableRow dispatch_table_row(std::move(binding));
   dispatch_table_row.callable_type_ =
       &ctx->type_of(&bs.seq_->back()->before_.at(0))->as<type::Callable>();

   if (auto obs = dispatch_table_row.SetTypes(
           dispatch_table_row.callable_type_->as<type::Function>().input,
           params, args, ctx);
       obs.obstructed()) {
     return obs;
   }

   return dispatch_table_row;
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

  InferenceState state(&new_ctx);
  if (!fn_lit->sorted_params_.empty() &&
      fn_lit->sorted_params_.back()->type_expr) {
    state.match_queue_.emplace(fn_lit->sorted_params_.back()->type_expr.get(),
                               args.at(0).type());
  }
  size_t last_success = 0;
  while (!state.match_queue_.empty()) {
    if (last_success >= state.match_queue_.size()) {
      NOT_YET("exit with a failure");
    }
    auto [e, t] = state.match_queue_.front();
    state.match_queue_.pop();
    if (e->InferType(t, &state)) {
      last_success = 0;
    } else {
      ++last_success;
      state.match_queue_.emplace(e, t);
    }
  }

  for (auto const &[d, t] : state.matches_) {
    new_ctx.bound_constants_.constants_.emplace(d, ir::Val(t));
  }

  for (size_t i = 0; i < args.num_pos(); ++i) {
    if (!fn_lit->inputs_.at(i).value->const_) { continue; }
    new_ctx.bound_constants_.constants_.emplace(
        fn_lit->inputs_.at(i).value.get(),
        backend::Evaluate(args.at(i).get(), ctx)[0]);
  }

  args.ApplyWithIndex([&](auto &&index, type::Typed<Expression *> expr) {
    size_t input_index = 0;
    if constexpr (std::is_same_v<std::decay_t<decltype(index)>, size_t>) {
      input_index = index;
    } else {
      input_index = fn_lit->inputs_.lookup_[index];
    }
    Declaration *decl = fn_lit->inputs_.at(input_index).value.get();
    if (!decl->const_) { return; }
    new_ctx.bound_constants_.constants_.emplace(
        decl, backend::Evaluate(expr.get(), ctx)[0]);
  });

  // TODO order these by their dependencies
  for (auto &[name, index] : fn_lit->inputs_.lookup_) {
    if (index < args.num_pos()) { continue; }
    if (!args.at_or_null(std::string(name))) { continue; }
    auto *decl = fn_lit->inputs_.at(index).value.get();

    decl->init_val->VerifyType(&new_ctx);
    new_ctx.bound_constants_.constants_.emplace(
        decl, backend::Evaluate(decl->init_val.get(), &new_ctx)[0]);
  }

  // TODO named arguments too.
  auto *fn_type = &ASSERT_NOT_NULL(fn_lit->VerifyTypeConcrete(&new_ctx).type_)
                       ->as<type::Callable>();
  binding.fn_.set_type(fn_type);

  DispatchTableRow dispatch_table_row(std::move(binding));
  dispatch_table_row.callable_type_ = ASSERT_NOT_NULL(fn_type);

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
    std::vector<type::Callable const *> const &callable_types) {
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
  std::vector<std::vector<type::Type const *>> out_types(num_outs);
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
  std::vector<type::Type const *> combined_outputs;
  combined_outputs.reserve(out_types.size());
  for (auto &ts : out_types) {
    combined_outputs.push_back(type::Var(std::move(ts)));
  }
  return type::Tup(std::move(combined_outputs));
}

std::pair<DispatchTable, type::Type const *> DispatchTable::Make(
    FnArgs<type::Typed<Expression *>> const &args,
    OverloadSet const &overload_set, Context *ctx) {
  DispatchTable table;

  // TODO Immovable default arguments are not handled here, nor can they be
  // because we don't know what defaults might be used until we look at each
  // particular overload.
  bool error = false;
  args.Apply([&](type::Typed<Expression *> const &e) {
    if (!e.type()->IsMovable()) {
      table.generic_failure_reasons_.emplace_back(e.type()->to_string() +
                                                  " is immovable.");
      error = true;
    }
  });
  if (error) { return std::pair{std::move(table), nullptr}; }

  std::vector<type::Callable const *> precise_callable_types;
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

    maybe_dispatch_table_row->binding_.fn_.set_type(
        maybe_dispatch_table_row->callable_type_);
    // TODO don't ned this as a field on the dispatchtablerow.
    precise_callable_types.push_back(maybe_dispatch_table_row->callable_type_);
    table.bindings_.emplace_back(
        std::move(maybe_dispatch_table_row->call_arg_types_),
        std::move(maybe_dispatch_table_row->binding_));
  }

  // TODO this won't work with generics. Need to get the info from the table
  // itself. Probably put in in a row.
  type::Type const *ret_type = ComputeRetType(precise_callable_types);

  return std::pair{std::move(table), ret_type};
}

template <typename IndexT>
static void AddType(IndexT &&index, type::Type const *t,
                    std::vector<FnArgs<type::Type const *>> *args) {
  if (auto *vt = t->if_as<type::Variant>()) {
    std::vector<FnArgs<type::Type const *>> new_args;
    for (auto *v : vt->variants_) {
      for (auto fnargs : *args) {
        if constexpr (std::is_same_v<std::decay_t<IndexT>, size_t>) {
          fnargs.pos_emplace(v);
        } else {
          fnargs.named_emplace(index, v);
        }
        new_args.push_back(std::move(fnargs));
      }
    }
    *args = std::move(new_args);
  } else {
    std::for_each(
        args->begin(), args->end(), [&](FnArgs<type::Type const *> &fnargs) {
          if constexpr (std::is_same_v<std::decay_t<IndexT>, size_t>) {
            fnargs.pos_emplace(t);
          } else {
            fnargs.named_emplace(index, t);
          }
        });
  }
}

static std::vector<FnArgs<type::Type const *>> Expand(
    FnArgs<type::Typed<Expression *>> const &typed_args) {
  std::vector<FnArgs<type::Type const *>> all_expanded_options(1);
  typed_args.ApplyWithIndex([&](auto &&index, type::Typed<Expression *> expr) {
    if (expr.get()->needs_expansion()) {
      for (auto *t : expr.type()->as<type::Tuple>().entries_) {
        AddType(index, t, &all_expanded_options);
      }
    } else {
      AddType(index, expr.type(), &all_expanded_options);
    }
  });

  return all_expanded_options;
}

type::Type const *DispatchTable::MakeOrLogError(
    Node *node, FnArgs<Expression *> const &args,
    OverloadSet const &overload_set, Context *ctx, bool repeated) {
  // TODO pull this out one more layer into the VerifyType call of node.
  auto typed_args = args.Transform([ctx](Expression *expr) {
    return type::Typed<Expression *>(expr, ASSERT_NOT_NULL(ctx->type_of(expr)));
  });

  auto [table, ret_type] = Make(typed_args, overload_set, ctx);
  if (table.bindings_.empty()) {
    // TODO what about operators?
    ctx->error_log()->NoCallMatch(node->span, table.generic_failure_reasons_,
                                table.failure_reasons_);
    return nullptr;
  }

  auto expanded     = Expand(typed_args);
  auto new_end_iter = std::remove_if(
      expanded.begin(), expanded.end(),
      [&](FnArgs<type::Type const *> const &fnargs) {
        return std::any_of(
            table.bindings_.begin(), table.bindings_.end(),
            [&fnargs](auto const &kv) { return Covers(kv.first, fnargs); });
      });
  expanded.erase(new_end_iter, expanded.end());
  if (!expanded.empty()) {
    ctx->error_log()->MissingDispatchContingency(node->span, expanded);
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
    type::Type const *ret_type, std::vector<ir::Val> *outgoing_regs,
    std::unordered_map<Expression *, ir::Results const *> const
        &expr_map,
    Binding const &binding, Context *ctx) {
  auto callee = [&] {
    Context fn_ctx(ctx->mod_);  // TODO this might be the wrong module.
    fn_ctx.bound_constants_ = binding.bound_constants_;
    if (auto *d = binding.fn_.get()->if_as<ast::Declaration>()) {
      // Skipping into the declaration initial value, which is kind of a hack,
      // but basically that's all we've verifyied the type for in this context.
      if (d->const_) {
        return ASSERT_NOT_NULL(d->init_val.get())
            ->EmitIr(&fn_ctx)
            .get<ir::AnyFunc>(0);
      }
    }
    return binding.fn_.get()->EmitIr(&fn_ctx).get<ir::AnyFunc>(0);
  }();

  if (!binding.const_) {
    if (!binding.fn_.get()->is<Declaration>() ||
        !binding.fn_.get()->as<Declaration>().is_fn_param_) {
      if (callee.is_reg_) {
        callee = ir::Load<ir::AnyFunc>(callee.reg_, binding.fn_.type());
      }
    }
  }

  // After the last check, if you pass, you should dispatch
  FnParams<Expression *> *const_params = nullptr;
  if (!callee.is_reg_ && callee.val_.is_fn()) {
    const_params = &(callee.val_.func()->params_);
  }

  ir::Arguments call_args{
      &binding.fn_.type()->as<type::Callable>(),
      binding.arg_res_.Results(const_params, expr_map, ctx)};

  ir::OutParams outs;

  // TODO don't copy the vector.
  std::vector<type::Type const *> out_types;
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

      ASSERT(expected_return_type, InheritsFrom<type::Variant>());
      ir::Store(return_type,
                ir::VariantType(std::get<ir::Register>(out_reg->value)));
      outs.AppendLoc(ir::VariantValue(return_type,
                                      std::get<ir::Register>(out_reg->value)));
    };

    if (ret_type->is<type::Tuple>()) {
      ASSERT(ret_type->as<type::Tuple>().size() == out_types.size());
      for (size_t i = 0; i < out_types.size(); ++i) {
        MakeRegister(out_types.at(i),
                     ret_type->as<type::Tuple>().entries_.at(i),
                     &outgoing_regs->at(i));
      }
    } else {
      MakeRegister(out_types.at(0), ret_type, &outgoing_regs->at(0));
    }
  }

  ir::Call(callee, std::move(call_args), std::move(outs));
}

static ir::RegisterOr<bool> EmitVariantMatch(ir::Register needle,
                                             type::Type const *haystack) {
  auto runtime_type = ir::Load<type::Type const *>(ir::VariantType(needle));

  if (haystack->is<type::Variant>()) {
    // TODO I'm fairly confident this will work, but it's also overkill because
    // we may already know this type matches if one variant is a subset of the
    // other.
    auto landing = ir::Func::Current->AddBlock();

    std::unordered_map<ir::BlockIndex, ir::RegisterOr<bool>> phi_map;
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
  ASSERT(big.num_pos() == small.num_pos());
  for (size_t i = 0; i < big.num_pos(); ++i) {
    if (big.at(i) == small.at(i)) { continue; }
    if (auto *vt = big.at(i)->if_as<type::Variant>()) {
      if (vt->contains(small.at(i))) { continue; }
    }
    return false;
  }

  for (auto const &[name, t] : small.named()) {
    if (type::Type const *const *big_t = big.at_or_null(name)) {
      if (t == *big_t) { continue; }
      if (auto *vt = (*big_t)->if_as<type::Variant>()) {
        if (vt->contains(t)) { continue; }
      }
    }
    return false;
  }

  return true;
}

static ir::BlockIndex CallLookupTest(
    FnArgs<std::pair<Expression *, ir::Results>> const &args,
    FnArgs<type::Type const *> const &call_arg_type, Context *ctx) {
  // Generate code that attempts to match the types on each argument (only
  // check the ones at the call-site that could be variants).

  // TODO enable variant dispatch on arguments that got expanded.
  auto next_binding = ir::Func::Current->AddBlock();

  args.ApplyWithIndex(
      [&](auto &&index,
          std::pair<Expression *, ir::Results> const &expr_and_val) {
        type::Type const *call_type = nullptr;
        auto const &[expr, val]     = expr_and_val;
        if (!ctx->type_of(expr)->is<type::Variant>()) { return; }

        if constexpr (std::is_same_v<std::decay_t<decltype(index)>, size_t>) {
          call_type = call_arg_type.at(index);
        } else {
          type::Type const *const *t = call_arg_type.at_or_null(index);
          if (!t) { return; }
          call_type = *t;
        }

        ir::BasicBlock::Current = ir::EarlyExitOn<false>(
            next_binding, EmitVariantMatch(val.get<ir::Reg>(0), call_type));
      });

  return next_binding;
}

ir::Results DispatchTable::EmitCall(
    FnArgs<std::pair<Expression *, ir::Results>> const &args,
    type::Type const *ret_type, Context *ctx) const {
  ASSERT(bindings_.size() != 0u);
  std::unordered_map<Expression *, ir::Results const *> expr_map;
  args.Apply([&expr_map](std::pair<Expression *, ir::Results> const &arg) {
    expr_map[arg.first] = &arg.second;
  });

  std::vector<ir::Val> out_regs;
  if (ret_type->is<type::Tuple>()) {
    out_regs.reserve(ret_type->as<type::Tuple>().size());
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
    const auto &[call_arg_type, binding] = *bindings_.begin();
    EmitOneCallDispatch(ret_type, &out_regs, expr_map, binding, ctx);
    return ir::Results::FromVals(out_regs);
  }

  // TODO push void out of here.
  size_t num_rets =
      ret_type->is<type::Tuple>() ? ret_type->as<type::Tuple>().size() : 1;

  std::vector<std::unordered_map<ir::BlockIndex, ir::Results>> result_phi_args(
      num_rets);

  auto landing_block = ir::Func::Current->AddBlock();

  auto iter = bindings_.begin();
  ASSERT(iter != bindings_.end());
  for (size_t i = 0; i < bindings_.size() - 1; ++i, ++iter) {
    const auto &[call_arg_type, binding] = *iter;
    auto next_binding = CallLookupTest(args, call_arg_type, ctx);
    size_t j          = 0;

    EmitOneCallDispatch(ret_type, &out_regs, expr_map, binding, ctx);
    for (const auto &out_reg : out_regs) {
      result_phi_args.at(j)[ir::BasicBlock::Current] =
          ir::Results::FromVals({out_reg});
      ++j;
    }
    ASSERT(j == num_rets);

    ir::UncondJump(landing_block);
    ir::BasicBlock::Current = next_binding;
  }

  const auto &[call_arg_type, binding] = *iter;
  size_t j                             = 0;
  EmitOneCallDispatch(ret_type, &out_regs, expr_map, binding, ctx);
  for (const auto &out_reg : out_regs) {
    result_phi_args.at(j)[ir::BasicBlock::Current] =
        ir::Results::FromVals({out_reg});
    ++j;
  }
  ASSERT(j == num_rets);

  ir::UncondJump(landing_block);
  ir::BasicBlock::Current = landing_block;

  switch (num_rets) {
    case 0: return ir::Results{};
    case 1:
      ASSERT(ret_type != type::Void());
      return ir::MakePhi(ret_type,
                         ir::Phi(ret_type->is_big() ? Ptr(ret_type) : ret_type),
                         result_phi_args[0]);
    default: {
      ir::Results results;
      const auto &tup_entries = ret_type->as<type::Tuple>().entries_;
      for (size_t i = 0; i < num_rets; ++i) {
        const type::Type *single_ret_type = tup_entries[i];
        ASSERT(single_ret_type != type::Void());
        results.append(
            ir::MakePhi(single_ret_type,
                        ir::Phi(single_ret_type->is_big() ? Ptr(single_ret_type)
                                                          : single_ret_type),
                        result_phi_args[i]));
      }
      return results;
    } break;
  }
  UNREACHABLE();
}

}  // namespace ast
