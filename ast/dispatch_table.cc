#include "ast/dispatch_table.h"

#include <variant>

#include "absl/algorithm/container.h"
#include "absl/strings/str_cat.h"
#include "ast/block_literal.h"
#include "ast/builtin_fn.h"
#include "ast/call.h"
#include "ast/function_literal.h"
#include "ast/match_declaration.h"
#include "backend/eval.h"
#include "base/expected.h"
#include "core/scope.h"
#include "ir/block.h"
#include "ir/cmd.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "misc/context.h"
#include "misc/module.h"
#include "type/cast.h"
#include "type/function.h"
#include "type/generic_struct.h"
#include "type/pointer.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace ast {
std::pair<DispatchTable, type::Type const *> DispatchTable::Make(
     core::FnArgs<type::Typed<Expression *>> const &args,
     OverloadSet const &overload_set, Context *ctx) {
  NOT_YET();
}

template <typename IndexT>
static void AddType(IndexT &&index, type::Type const *t,
                    std::vector<core::FnArgs<type::Type const *>> *args) {
  if (auto *vt = t->if_as<type::Variant>()) {
    std::vector<core::FnArgs<type::Type const *>> new_args;
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
        args->begin(), args->end(),
        [&](core::FnArgs<type::Type const *> &fnargs) {
          if constexpr (std::is_same_v<std::decay_t<IndexT>, size_t>) {
            fnargs.pos_emplace(t);
          } else {
            fnargs.named_emplace(index, t);
          }
        });
  }
 }

static std::vector<core::FnArgs<type::Type const *>> ExpandAllFnArgs(
    core::FnArgs<std::pair<Expression *, VerifyResult>> const &args) {
  std::vector<core::FnArgs<type::Type const *>> all_expanded_options(1);
  args.ApplyWithIndex(
      [&](auto &&index, std::pair<Expression *, VerifyResult> const &p) {
        if (p.first->needs_expansion()) {
          for (auto *t : p.second.type_->as<type::Tuple>().entries_) {
            AddType(index, t, &all_expanded_options);
          }
        } else {
          AddType(index, p.second.type_, &all_expanded_options);
        }
      });

  return all_expanded_options;
}

// Small contains expanded arguments (no variants).
static bool Covers(core::FnParams<type::Typed<Expression *>> const &params,
                   core::FnArgs<type::Type const *> const &args) {
  if (params.size() < args.size()) { return false; }

  for (size_t i = 0; i < args.pos().size(); ++i) {
    if (!type::CanCast(args.pos().at(i), params.at(i).value.type())) {
      return false;
    }
  }

  for (auto const &[name, type] : args.named()) {
    auto *index = params.at_or_null(name);
    if (index == nullptr) { return false; }
    if (!type::CanCast(type, params.at(*index).value.type())) { return false; }
  }

  for (size_t i = args.pos().size(); i < params.size(); ++i) {
    auto const& param = params.at(i);
    if (param.flags & core::HAS_DEFAULT) { continue; }
    if (args.at_or_null(std::string{param.name}) == nullptr) { return false; }
  }

  return true;
}

static base::expected<core::FnParams<type::Typed<Expression *>>>
MatchArgsToParams(
    core::FnParams<type::Typed<Expression *>> const &params,
    core::FnArgs<std::pair<Expression *, VerifyResult>> const &args) {
  if (args.pos().size() > params.size()) {
    return base::unexpected(absl::StrCat(
        "Too many arguments provided (", args.size(),
        ") but expression is only callable with at most ", params.size()));
  }

  core::FnParams<type::Typed<Expression *>> matched_params;
  size_t param_index = 0;
  for (auto const &[expr, verify_result] : args.pos()) {
    auto const &param = params.at(param_index);
    type::Type const *meet =
        type::Meet(verify_result.type_, param.value.type());
    if (!meet) {
      return base::unexpected(absl::StrCat(
          "Failed to match argument to parameter at position ", param_index));
    }
    // Note: I'm intentionally not taking any flags here.
    matched_params.append(
        param.name, type::Typed(static_cast<Expression *>(nullptr), meet));
    ++param_index;
  }

  // TODO currently bailing on first error. Probably better to determine all
  // errors.
  for (size_t i = param_index; i < params.size(); ++i) {
    auto const &param   = params.at(i);
    auto *expr_and_verify_result = args.at_or_null(std::string{param.name});
    if (expr_and_verify_result == nullptr) {
      if ((param.flags & core::HAS_DEFAULT) == 0) {
        if (param.name.empty()) {
          return base::unexpected(absl::StrCat(
              "No argument provided for anonymous parameter at position `", i,
              "`"));

        } else {
          return base::unexpected(absl::StrCat(
              "No argument provided for parameter `", param.name, "`"));
        }
      } else {
        matched_params.append(param.name, param.value, param.flags);
        continue;
      }
    } else {
      auto *meet =
          type::Meet(expr_and_verify_result->second.type_, param.value.type());
      if (!meet) {
        // TODO explain why types don't match.
        return base::unexpected(absl::StrCat(
            "Failed to match argument to parameter named `", param.name, "`"));
      }
      // Note: I'm intentionally not taking any flags here.
      matched_params.append(
          param.name, type::Typed(static_cast<Expression *>(nullptr), meet));
    }
  }

  for (auto const &[name, unused] : args.named()) {
    auto *index = params.at_or_null(name);
    if (index == nullptr) {
      return base::unexpected(
          absl::StrCat("No parameter matching argument named `", name, "`"));
    }
    if (*index < args.pos().size()) {
      // TODO better error message.
      return base::unexpected(absl::StrCat(
          "Argument named `", name, "` shadows positional argument ", *index));
    }
  }

  return  matched_params;
}

static base::expected<DispatchTable::Row> OverloadParams(
    Overload const &overload,
    core::FnArgs<std::pair<Expression *, VerifyResult>> const &args,
    Context *ctx) {
  // These are the parameters for the actual overload that will be potentially
  // selected. In particular, if we have two overloads:
  //
  //   f :: (A | B) -> X = ...
  //   f :: (C | D) -> Y = ...
  //
  // And we call it with an object of type (A | C), then these parameters will
  // be (A | B) and (C | D) on the two loop iterations. This is to distinct from
  // how it actually gets called which would be with the parameters A and C
  // respectively.

  auto result =
      *ASSERT_NOT_NULL(ctx->prior_verification_attempt(overload.expr));
  if (!result.type_->is<type::Callable>() && result.type_ != type::Generic &&
      result.type_ != type::Block && result.type_ != type::OptBlock &&
      result.type_ != type::RepBlock) {
    // Figure out who should have verified this. Is it guaranteed to be
    // covered by shadowing checks? What if the overload isn't a declaration
    // so there aren't any shadowing checks?
    base::Log() << result.type_->to_string();
    NOT_YET();
  }

  if (result.const_) {
    if (result.type_ == type::Generic) {
      auto *fn_lit =
          backend::EvaluateAs<ast::FunctionLiteral *>(overload.expr, ctx);

      core::FnParams<type::Typed<Expression *>> params(fn_lit->inputs_.size());
      for (auto *decl : fn_lit->sorted_params_) {
        // TODO skip decls that are not parameters.
        size_t param_index = fn_lit->decl_to_param_.at(decl);
        auto const& param = fn_lit->inputs_.at(param_index);

        auto result = decl->VerifyType(ctx);
        if (!result.ok()) { NOT_YET(); }

        if (!param.value->const_) {
          params.set(param_index, core::Param<type::Typed<Expression *>>{
                                      param.name,
                                      type::Typed<Expression *>(
                                          param.value.get(),
                                          ctx->type_of(param.value.get())),
                                      param.flags});
        } else {
          if (param_index < args.pos().size()) {
            auto [arg_expr, verify_result] = args.pos().at(param_index);
            type::Type const *decl_type    = ctx->type_of(param.value.get());
            if (!type::CanCast(verify_result.type_, decl_type)) {
              return base::unexpected(
                  absl::StrCat("TODO good error message couldn't match type ",
                               decl_type->to_string(), " to ",
                               verify_result.type_->to_string()));
            }

            auto buf = backend::EvaluateToBuffer(
                type::Typed(arg_expr, verify_result.type_), ctx);
            auto [data_offset, num_bytes] =
                std::get<std::pair<size_t, core::Bytes>>(
                    ctx->current_constants_.reserve_slot(param.value.get(), decl_type));
            // TODO you haven't done the cast yet! And you didn't even check
            // about implicit casts.
            ctx->current_constants_.set_slot(data_offset, buf.raw(0),
                                             num_bytes);
            params.set(param_index, core::Param<type::Typed<Expression *>>{
                                        param.name,
                                        type::Typed<Expression *>(
                                            param.value.get(), decl_type),
                                        param.flags});
          } else {
            if (auto *arg = args.at_or_null(param.value->id_)) {
              type::Type const *decl_type = ctx->type_of(param.value.get());
              if (!type::CanCast(arg->second.type_, decl_type)) {
                return base::unexpected(
                    absl::StrCat("TODO good error message couldn't match type ",
                                 decl_type->to_string(), " to ",
                                 arg->second.type_->to_string()));
              }

              auto buf = backend::EvaluateToBuffer(
                  type::Typed(arg->first, decl_type), ctx);
            auto [data_offset, num_bytes] =
                std::get<std::pair<size_t, core::Bytes>>(
                    ctx->current_constants_.reserve_slot(param.value.get(),
                                                         decl_type));
            // TODO you haven't done the cast yet! And you didn't even check
            // about implicit casts.
            ctx->current_constants_.set_slot(data_offset, buf.raw(0),
                                             num_bytes);
            params.set(param_index, core::Param<type::Typed<Expression *>>{
                                        param.name,
                                        type::Typed<Expression *>(
                                            param.value.get(),
                                            ctx->type_of(param.value.get())),
                                        param.flags});

            } else {
              if (param.flags & core::HAS_DEFAULT) {
                type::Type const *decl_type =
                    ctx->type_of(param.value.get());

                // TODO you haven't done the cast from init_val to declared type
                auto buf = backend::EvaluateToBuffer(
                    type::Typed(decl->init_val.get(), decl_type), ctx);
                auto [data_offset, num_bytes] =
                    std::get<std::pair<size_t, core::Bytes>>(
                        ctx->current_constants_.reserve_slot(param.value.get(),
                                                             decl_type));
                ctx->current_constants_.set_slot(data_offset, buf.raw(0),
                                                 num_bytes);
                // TODO should I be setting this parameter?

                params.set(param_index,
                           core::Param<type::Typed<Expression *>>{
                               param.name,
                               type::Typed<Expression *>(decl->init_val.get(),
                                                         decl_type),
                               param.flags});
              } else {
                return base::unexpected(
                    "TODO good error message. needed default parameter but "
                    "none provided.");
              }
            }
          }
        }
      }

      auto *old_constants = std::exchange(
          ctx->constants_, ctx->insert_constants(ctx->current_constants_));
      base::defer d([&]() { ctx->constants_ = old_constants; });
      // TODO errors?
      auto *fn_type = ASSERT_NOT_NULL(fn_lit->VerifyTypeConcrete(ctx).type_);
      return DispatchTable::Row{std::move(params),
                                &fn_type->as<type::Function>(),
                                backend::EvaluateAs<ir::AnyFunc>(fn_lit, ctx)};
    } else {
      if (result.type_ == type::Block || result.type_ == type::OptBlock ||
          result.type_ == type::RepBlock) {
        ir::Block b = backend::EvaluateAs<ir::Block>(overload.expr, ctx);
        // TODO each of these is wrong (e.g., not taking parameters?
        if (b == ir::Block::Start()) {
          return DispatchTable::Row{core::FnParams<type::Typed<Expression *>>{},
                                    type::Func({}, {}), nullptr};

        } else if (b == ir::Block::Exit()) {
          return DispatchTable::Row{core::FnParams<type::Typed<Expression *>>{},
                                    type::Func({}, {}),
                                    reinterpret_cast<Expression *>(0x1)};

        } else {
          return DispatchTable::Row{core::FnParams<type::Typed<Expression *>>{},
                                    type::Func({}, {}),
                                    const_cast<ast::BlockLiteral *>(b.get())};
        }
      }

      ir::AnyFunc fn = backend::EvaluateAs<ir::AnyFunc>(overload.expr, ctx);

      if (fn.is_fn()) {
        return DispatchTable::Row{fn.func()->params_, fn.func()->type_, fn};
      } else {
        if (auto *fn_type = fn.foreign().type()->if_as<type::Function>()) {
          // TODO foreign functions should be allowed named and default
          // parameters.
          return DispatchTable::Row{fn_type->AnonymousFnParams(), fn_type, fn};
        } else {
          UNREACHABLE();
        }
      }
    }
  } else {
    if (result.type_ == type::Generic) {
      UNREACHABLE();
    } else if (auto *fn_type = result.type_->if_as<type::Function>()) {
      return DispatchTable::Row{fn_type->AnonymousFnParams(), fn_type,
                                overload.expr};
    } else if (result.type_ == type::Block) {
      NOT_YET();
    } else {
      UNREACHABLE();
    }
  }
}

static size_t NumOutputs(std::vector<DispatchTable::Row> const &rows) {
  if (rows.empty()) { return std::numeric_limits<size_t>::max(); }

  size_t expected = rows.front().type->output.size();
  return absl::c_all_of(rows,
                        [expected](DispatchTable::Row const &row) {
                          return row.type->output.size() == expected;
                        })
             ? expected
             : std::numeric_limits<size_t>::max();
}

static std::vector<type::Type const *> ReturnTypes(
    size_t num_outputs, std::vector<DispatchTable::Row> const &rows) {
  if (num_outputs == 0) { return {}; }
  std::vector<std::vector<type::Type const *>> transposed(num_outputs);
  for (auto const &row : rows) {
    ASSERT(row.type->output.size() == num_outputs);
    for (size_t i = 0; i < num_outputs; ++i) {
      transposed.at(i).push_back(row.type->output.at(i));
    }
  }

  std::vector<type::Type const *> result;
  result.reserve(num_outputs);
  for (auto &types : transposed) {
    result.push_back(type::Var(std::move(types)));
  }
  return result;
}

// TODO It is unsafe to access `expr` because it may have some low bits set.
// Change it to uintptr_t.
VerifyResult VerifyDispatch(
    ExprPtr expr, OverloadSet const &os,
    core::FnArgs<std::pair<Expression *, VerifyResult>> const &args,
    Context *ctx) {
  DispatchTable table;
  bool is_const = true;
  absl::flat_hash_map<Expression const *, std::string> failure_reasons;
  for (Overload const &overload : os) {
    is_const &= overload.result.const_;
    auto expected_row = OverloadParams(overload, args, ctx);
    if (!expected_row.has_value()) {
      failure_reasons.emplace(overload.expr,
                              std::move(expected_row).error().to_string());
      continue;
    }
    auto match = MatchArgsToParams(expected_row->params, args);
    if (!match.has_value()) {
      failure_reasons.emplace(overload.expr,
                              std::move(match).error().to_string());
      continue;
    }

    expected_row->params = *std::move(match);
    table.bindings_.push_back(*std::move(expected_row));
  }

  size_t num_outputs = NumOutputs(table.bindings_);
  if (num_outputs == std::numeric_limits<size_t>::max()) {
    return ctx->set_result(expr, VerifyResult::Error());
  }
  table.return_types_ = ReturnTypes(num_outputs, table.bindings_);
  auto *tup = type::Tup(table.return_types_);

  auto expanded_fnargs = ExpandAllFnArgs(args);
  expanded_fnargs.erase(
      std::remove_if(expanded_fnargs.begin(), expanded_fnargs.end(),
                     [&](core::FnArgs<type::Type const *> const &fnargs) {
                       return absl::c_any_of(
                           table.bindings_,
                           [&fnargs](DispatchTable::Row const &row) {
                             return Covers(row.params, fnargs);
                           });
                     }),
      expanded_fnargs.end());
  if (!expanded_fnargs.empty()) {
    // TODO log an error
    // ctx->error_log()->MissingDispatchContingency(node->span, expanded_fnargs);
    return VerifyResult::Error();
  }

  ctx->set_dispatch_table(expr, std::move(table));

  // TODO this assumes we only have one return value or that we're returning a
  // tuple. So, e.g., you don't get the benefit of A -> (A, A) and B -> (A, B)
  // combining into (A | B) -> (A, A | B).
  return ctx->set_result(expr, VerifyResult(tup, is_const));
}

static ir::RegisterOr<bool> EmitVariantMatch(ir::Reg needle,
                                             type::Type const *haystack) {
  auto runtime_type = ir::Load<type::Type const *>(ir::VariantType(needle));

  if (auto *haystack_var = haystack->if_as<type::Variant>()) {
    // TODO I'm fairly confident this will work, but it's also overkill because
    // we may already know this type matches if one variant is a subset of the
    // other.
    auto landing = ir::CompiledFn::Current->AddBlock();

    absl::flat_hash_map<ir::BlockIndex, ir::RegisterOr<bool>> phi_map;
    for (type::Type const *v : haystack_var->variants_) {
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

static ir::BlockIndex EmitDispatchTest(
    core::FnParams<type::Typed<ast::Expression *>> const &params,
    core::FnArgs<std::pair<Expression *, ir::Results>> const &args,
    Context *ctx) {
  auto next_binding = ir::CompiledFn::Current->AddBlock();

  for (size_t i = 0; i < params.size(); ++i) {
    const auto &param = params.at(i);
    if (param.flags & core::HAS_DEFAULT) { continue; }
    auto const &[expr, val] =
        (i < args.pos().size()) ? args.at(i) : args.at(std::string{param.name});
    auto *expr_var = ctx->type_of(expr)->if_as<type::Variant>();
    if (!expr_var) { continue; }
    ir::BasicBlock::Current = ir::EarlyExitOn<false>(
        next_binding,
        EmitVariantMatch(val.get<ir::Reg>(0), param.value.type()));
  }
  return next_binding;
}

// TODO inline_results is a hacky solution to this problem and you should think
// about solving it robustly. Return ir::Results for both the in and out-of-line
// cases.
template <bool Inline>
static void EmitOneCall(
    DispatchTable::Row const &row,
    core::FnArgs<std::pair<Expression *, ir::Results>> const &args,
    std::vector<type::Type const *> const &return_types,
    std::vector<std::variant<
        ir::Reg, absl::flat_hash_map<ir::BlockIndex, ir::Results> *>> *outputs,
    absl::flat_hash_map<ir::Block, ir::BlockIndex> const &block_map,
    ir::Results *inline_results, Context *ctx) {
  // TODO look for matches
  ir::RegisterOr<ir::AnyFunc> fn = std::visit(
      [&](auto f) -> ir::RegisterOr<ir::AnyFunc> {
        using T = std::decay_t<decltype(f)>;
        if constexpr (std::is_same_v<T, ir::AnyFunc>) {
          return f;
        } else {
          // TODO must `f` always be a declaration?
          return ir::Load(ctx->addr(&f->template as<Declaration>()), row.type);
        }
      },
      row.fn);
  // TODO this feels super hacky. ANd wasteful to compute `fn` twice.
  if (!Inline && !fn.is_reg_ && fn.val_.is_fn() &&
      fn.val_.func()->must_inline_) {
    return EmitOneCall<true>(row, args, return_types, outputs, block_map,
                             inline_results, ctx);
  }

  ir::Results arg_results;
  size_t i = 0;
  ASSERT(row.params.size() >= args.pos().size());
  for (auto const &[expr, results] : args.pos()) {
    // TODO Don't re-lookup the type of this expression. You should know it
    // already.
    arg_results.append(row.params.at(i++).value.type()->PrepareArgument(
        ASSERT_NOT_NULL(ctx->type_of(expr)), results, ctx));
  }

  for (; i < row.params.size(); ++i) {
    auto const &param = row.params.at(i);
    auto *arg         = args.at_or_null(std::string{param.name});
    if (!arg && (param.flags & core::HAS_DEFAULT)) {
      arg_results.append(param.value.get()->EmitIr(ctx));
    } else {
      auto const &[expr, results] = *arg;
      arg_results.append(param.value.type()->PrepareArgument(ctx->type_of(expr),
                                                             results, ctx));
    }
  }

  if constexpr (Inline) {
    ASSERT(fn.is_reg_ == false);
    if (fn.val_.is_fn()) {
      auto *prev_inline_map = std::exchange(ctx->inline_, nullptr);
      base::defer d([&]() { ctx->inline_ = prev_inline_map; });
      auto *func = ASSERT_NOT_NULL(fn.val_.func());
      if (func->work_item != nullptr) { (*func->work_item)(); }
      ASSERT(func->work_item == nullptr);

      *inline_results =
          ir::CallInline(func, ir::Arguments{row.type, arg_results}, block_map);
    } else {
      NOT_YET();
    }

  } else {
    ir::OutParams out_params;

    auto call_block = ir::CompiledFn::Current->AddBlock();

    size_t j = 0;
    for (type::Type const *ret_type : row.type->output) {
      if (ret_type->is_big()) {
        ir::Reg reg = std::get<ir::Reg>(outputs->at(j++));
        // TODO this seems like something that should be shareable with the
        // type-based assignment/initialization code.
        NOT_YET(reg);
      } else {
        std::visit(
            [&](auto out) {
              if constexpr (std::is_same_v<std::decay_t<decltype(out)>,
                                           ir::Reg>) {
                // This specific function returns something small enough to fit
                // in a register, but combined with all the other possible
                // dispatches, we need more space.

                // There is no need to call the destructor on this variant that
                // we're overwriting because it has not been initialized yet.
                ir::Store(ret_type, ir::VariantType(out));
                auto val = ir::VariantValue(ret_type, out);
                out_params.AppendLoc(val);
              } else {
                // Every function that may be dispatched to returns the same
                // type.
                ir::Reg out_reg = out_params.AppendReg(ret_type);
                out->emplace(call_block, ir::Results{out_reg});
              }
            },
            outputs->at(j++));
      }
    }

    ir::UncondJump(call_block);
    ir::BasicBlock::Current = call_block;

    ir::Call(fn, ir::Arguments{row.type, std::move(arg_results)},
             std::move(out_params));
  }
}

template <bool Inline>
static ir::Results EmitFnCall(
    DispatchTable const *table,
    core::FnArgs<std::pair<Expression *, ir::Results>> const &args,
    absl::flat_hash_map<ir::Block, ir::BlockIndex> const &block_map,
    Context *ctx) {
  // If an output to the function fits in a register we will create a phi node
  // for it on the landing block. Otherwise, we'll temporarily allocate stack
  // space for it and pass in an output pointer.
  size_t num_regs = absl::c_count_if(
      table->return_types_, [](type::Type const *t) { return !t->is_big(); });
  absl::flat_hash_map<ir::BlockIndex, ir::Results> result_phi_args[num_regs];

  // The vector of registers for all the outputs aggregated from all the
  // possible functions in the dispatch table.
  //
  // If the output is too big to fit in a register, the register is the address
  // of a temporarily stack-allocated slot for the return value. Otherwise,
  // this is the register corresponding to the output phi-node.
  std::vector<
      std::variant<ir::Reg, absl::flat_hash_map<ir::BlockIndex, ir::Results> *>>
      outputs;
  outputs.reserve(table->return_types_.size());

  size_t index_into_phi_args = 0;
  for (type::Type const *t : table->return_types_) {
    if (t->is_big()) {
      outputs.emplace_back(ir::TmpAlloca(t, ctx));
    } else {
      outputs.emplace_back(&result_phi_args[index_into_phi_args++]);
    }
  }

  auto landing_block = ir::CompiledFn::Current->AddBlock();

  ir::Results inline_results;
  for (size_t i = 0; i + 1 < table->bindings_.size(); ++i) {
    auto const &row   = table->bindings_.at(i);
    auto next_binding = EmitDispatchTest(row.params, args, ctx);

    EmitOneCall<Inline>(row, args, table->return_types_, &outputs, block_map,
                        &inline_results, ctx);

    ir::UncondJump(landing_block);
    ir::BasicBlock::Current = next_binding;
  }

  EmitOneCall<Inline>(table->bindings_.back(), args, table->return_types_,
                      &outputs, block_map, &inline_results, ctx);

  ir::UncondJump(landing_block);
  ir::BasicBlock::Current = landing_block;

  if constexpr (Inline) {
    return inline_results;
  } else {
    ir::Results results;
    for (size_t i = 0; i < table->return_types_.size(); ++i) {
      std::visit(
          [&](auto out) {
            if constexpr (std::is_same_v<std::decay_t<decltype(out)>,
                                         ir::Reg>) {
              // Return is large. We allocated a slot large enough ahead of time
              // and simply wrote to it. Thus, no phi node is necessary, we can
              // just return a pointer to the temporary allocation.
              results.append(out);
            } else {
              // Return is small enough to fit in a register, so we need to
              // create a phi node joining all the registers from all the
              // possible dispatches.
              type::Type const *ret_type = table->return_types_[i];
              results.append(ir::MakePhi(ret_type, ir::Phi(ret_type), *out));
            }
          },
          outputs[i]);
    }

    return results;
  }
}

ir::Results DispatchTable::EmitInlineCall(
    core::FnArgs<std::pair<Expression *, ir::Results>> const &args,
    absl::flat_hash_map<ir::Block, ir::BlockIndex> const &block_map,
    Context *ctx) const {
  return EmitFnCall<true>(this, args, block_map, ctx);
}

ir::Results DispatchTable::EmitCall(
    core::FnArgs<std::pair<Expression *, ir::Results>> const &args,
    Context *ctx) const {
  return EmitFnCall<false>(this, args, {}, ctx);
}

}  // namespace ast
