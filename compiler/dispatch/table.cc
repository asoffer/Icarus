#include "compiler/dispatch/table.h"

#include "ast/expression.h"
#include "base/debug.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/extract_params.h"
#include "compiler/extract_jumps.h"
#include "core/fn_params.h"
#include "ir/builder.h"
#include "type/cast.h"
#include "type/type.h"
#include "type/variant.h"

namespace compiler::internal {
namespace {
template <typename IndexT>
void AddType(IndexT &&index, type::Type const *t,
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

// TODO: Ideally we wouldn't create these all at once but rather iterate through
// the possibilities. Doing this the right way involves having sum and product
// iterators.
std::vector<core::FnArgs<type::Type const *>> ExpandedFnArgs(
    core::FnArgs<VerifyResult> const &fnargs) {
  std::vector<core::FnArgs<type::Type const *>> all_expanded_options(1);
  fnargs.ApplyWithIndex([&](auto &&index, compiler::VerifyResult r) {
    // TODO also maybe need the expression this came from to see if it needs
    // to be expanded.
    AddType(index, r.type(), &all_expanded_options);
  });

  return all_expanded_options;
}

std::pair<ir::Results, ir::OutParams> SetReturns(
    ExprData const &expr_data, absl::Span<type::Type const *> final_out_types) {
  auto const & [ type, params ] = expr_data;
  auto const &ret_types         = type->as<type::Function>().output;
  ir::Results results;
  ir::OutParams out_params;
  for (type::Type const *ret_type : ret_types) {
    if (ret_type->is_big()) {
      NOT_YET();
    } else {
      out_params.AppendReg(ret_type);
      results.append(out_params.regs_.back());
    }
  }
  return std::pair<ir::Results, ir::OutParams>(std::move(results),
                                               std::move(out_params));
}

ir::Results EmitCallOneOverload(
    Compiler *compiler, ast::Expression const *fn, ExprData const &data,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  auto const & [ type, params ] = data;
  std::vector<ir::Results> arg_results;
  // TODO prep args (if it's a variant, e.g.)
  for (auto arg : args.pos()) { arg_results.push_back(arg.get()); }
  for (size_t i = args.pos().size(); i < params.size(); ++i) {
    auto const &param = params.at(i);
    if (auto *arg = args.at_or_null(param.name)) {
      arg_results.push_back(arg->get());
    } else {
      arg_results.push_back(ir::Results{compiler->Visit(
          ASSERT_NOT_NULL(param.value.get()->init_val()), EmitValueTag{})});
    }
  }
  auto[out_results, out_params] = SetReturns(data, {});
  compiler->builder().Call(
      compiler->Visit(fn, EmitValueTag{}).get<ir::AnyFunc>(0),
      &compiler->type_of(fn)->as<type::Function>(), arg_results, out_params);
  return std::move(out_results);
}

template <typename TableType>
bool ParamsCoverArgs(core::FnArgs<VerifyResult> const &args,
                     TableType const &table) {
  auto expanded_fnargs = ExpandedFnArgs(args);
  for (auto const &expanded_arg : expanded_fnargs) {
    for (auto const & [ k, v ] : table) {
      bool callable =
          core::IsCallable(v.params, args,
                           [](VerifyResult arg,
                              type::Typed<ast::Declaration const *> param) {
                             return type::CanCast(arg.type(), param.type());
                           });
      if (callable) { goto next_expanded_arg; }
    }
    return false;
  next_expanded_arg:;
  }
  return true;
}

}  // namespace

base::expected<TableImpl> TableImpl::Verify(
    Compiler *compiler, ast::OverloadSet const &os,
    core::FnArgs<VerifyResult> const &args) {
  DEBUG_LOG("dispatch-verify")
  ("Verifying overload set with ", os.members().size(), " members.");

  // Keep a collection of failed matches around so we can give better
  // diagnostics.
  absl::flat_hash_map<ast::Expression const *, FailedMatch> failures;
  TableImpl table;
  for (ast::Expression const *overload : os.members()) {
    // TODO the type of the specific overload could *correctly* be null and we
    // need to handle that case.
    DEBUG_LOG("dispatch-verify")
    ("Verifying ", overload, ": ", overload->DebugString());
    auto result = MatchArgsToParams(ExtractParams(compiler, overload), args);
    if (not result) {
      failures.emplace(overload, result.error());
    } else {
      // TODO you also call compiler->type_of inside ExtractParams, so it's
      // probably worth reducing the number of lookups.
      table.table_.emplace(overload,
                           ExprData{compiler->type_of(overload), *result});
    }
  }

  if (not ParamsCoverArgs(args, table.table_)) { NOT_YET("log an error"); }
  return table;
}

std::vector<core::FnArgs<VerifyResult>> VerifyBlockNode(
    Compiler *compiler, ast::BlockNode const *node) {
  compiler->Visit(node, VerifyTypeTag{});

  ExtractJumps extractor;
  for (auto const *stmt : node->stmts()) { extractor.Visit(stmt); }

  auto yields = extractor.jumps(ExtractJumps::Kind::Yield);
  // TODO this setup is definitely wrong because it doesn't account for
  // multiple yields correctly. For example,
  //
  // ```
  //  result: int32 | bool = if (cond) then {
  //    yield 3
  //  } else if (other_cond) then {
  //    yield 4
  //  } else {
  //    yield true
  //  }
  //  ```
  std::vector<core::FnArgs<VerifyResult>> result;
  for (auto *yield : yields) {
    auto &back = result.emplace_back();
    // TODO actually fill a fnargs
    std::vector<std::pair<ast::Expression const *, VerifyResult>>
        local_pos_yields;
    for (auto *yield_expr : yields[0]->as<ast::YieldStmt>().exprs()) {
      back.pos_emplace(
          *ASSERT_NOT_NULL(compiler->prior_verification_attempt(yield_expr)));
    }
  }
  return result;
}

}  // namespace compiler::internal

namespace compiler {
base::expected<ScopeDispatchTable::JumpDispatchTable> ScopeDispatchTable::JumpDispatchTable::Verify(
    Compiler *compiler, ast::ScopeNode const *node,
    absl::Span<ir::Jump const *const> jumps,
    core::FnArgs<VerifyResult> const &args) {
  DEBUG_LOG("dispatch-verify")
  ("Verifying overload set with ", jumps.size(), " members.");

  // Keep a collection of failed matches around so we can give better
  // diagnostics.
  absl::flat_hash_map<ir::Jump const *, FailedMatch> failures;
  ScopeDispatchTable::JumpDispatchTable table;
  for (ir::Jump const *jump : jumps) {
    // TODO the type of the specific overload could *correctly* be null and
    // we need to handle that case.
    DEBUG_LOG("dispatch-verify")("Verifying ", jump);
    auto result = MatchArgsToParams(jump->params(), args);
    if (not result) {
      failures.emplace(jump, result.error());
    } else {
      // TODO you also call compiler->type_of inside ExtractParams, so it's
      // probably worth reducing the number of lookups.
      table.table_.emplace(jump, internal::ExprData{jump->type(), *result});
    }
  }

  if (not internal::ParamsCoverArgs(args, table.table_)) {
    NOT_YET("log an error");
  }
  return table;
}

type::Type const *FnCallDispatchTable::ComputeResultType(
    internal::TableImpl const &impl) {
  std::vector<std::vector<type::Type const *>> results;
  for (auto const & [ overload, expr_data ] : impl.table_) {
    auto const & [ type, fn_params ] = expr_data;
    DEBUG_LOG("dispatch-verify")
    ("Extracting return type for ", overload->DebugString(), " of type ",
     type->to_string());
    if (auto *fn_type = type->if_as<type::Function>()) {
      auto const &out_vec = fn_type->output;
      results.push_back(out_vec);
    } else if (type == type::Generic) {
      NOT_YET("log error");
    } else {
      NOT_YET();
    }
  }

  return type::MultiVar(results);
}

ir::Results FnCallDispatchTable::EmitCall(
    Compiler *compiler,
    core::FnArgs<type::Typed<ir::Results>> const &args) const {
  if (impl_.table_.size() == 1) {
    auto const & [ overload, expr_data ] = *impl_.table_.begin();
    return internal::EmitCallOneOverload(compiler, overload, expr_data, args);
  } else {
    NOT_YET();
  }
  return ir::Results{};
}

base::expected<ScopeDispatchTable> ScopeDispatchTable::Verify(
    Compiler *compiler, ast::ScopeNode const *node,
    absl::flat_hash_map<ir::Jump const *, ir::ScopeDef const *> inits,
    core::FnArgs<VerifyResult> const &args) {
  absl::flat_hash_map<ir::ScopeDef const *,
                      absl::flat_hash_map<ir::Jump const *, FailedMatch>>
      failures;
  ScopeDispatchTable table;
  for (auto[jump, scope] : inits) {
    auto result = MatchArgsToParams(jump->params(), args);
    if (not result) {
      failures[scope].emplace(jump, result.error());
    } else {
      table.init_table_[scope].emplace(jump, *result);
    }
  }

  auto expanded_fnargs = internal::ExpandedFnArgs(args);
  expanded_fnargs.erase(
      std::remove_if(
          expanded_fnargs.begin(), expanded_fnargs.end(),
          [&](core::FnArgs<type::Type const *> const &fnargs) {
            for (auto const & [ k, v ] : table.init_table_) {
              for (auto const & [ init, params ] : v) {
                if (core::IsCallable(
                        params, fnargs,
                        [](type::Type const *arg,
                           type::Typed<ast::Declaration const *> param) {
                          return type::CanCast(arg, param.type());
                        })) {
                  return true;
                }
              }
            }
            return false;
          }),
      expanded_fnargs.end());
  if (not expanded_fnargs.empty()) { NOT_YET("log an error"); }

  // If there are any scopes in this overload set that do not have blocks of the
  // corresponding names, we should exit.
  for (auto[scope_def, _] : table.init_table_) {
    auto &block_tables = table.block_tables_[scope_def];
    for (auto const &block : node->blocks()) {
      DEBUG_LOG("ScopeNode")
      ("Verifying dispatch for block `", block.name(), "`");
      auto const *block_def = scope_def->block(block.name());
      if (not block_def) { NOT_YET("log an error"); }
      auto block_results = internal::VerifyBlockNode(compiler, &block);
      DEBUG_LOG("ScopeNode")("    ", block_results);
      if (block_results.empty()) {
        // There are no relevant yield statements
        DEBUG_LOG("ScopeNode")("    ... empty block results");

        ASSIGN_OR(
            continue,  //
            auto table,
            JumpDispatchTable::Verify(compiler, node, block_def->after_, {}));
        bool success = block_tables.emplace(&block, std::move(table)).second;
        static_cast<void>(success);
        ASSERT(success == true);
      } else {
        for (auto const &fn_args : block_results) { NOT_YET(); }
      }
      DEBUG_LOG("ScopeNode")("    ... done.");
    }
  }

  return table;
}

}  // namespace compiler
