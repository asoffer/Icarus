#include "compiler/dispatch/scope_table.h"

#include "ast/expression.h"
#include "base/debug.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "compiler/extract_jumps.h"
#include "core/fn_params.h"
#include "ir/builder.h"
#include "type/cast.h"
#include "type/type.h"
#include "type/variant.h"

namespace compiler::internal {

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

  if (not ParamsCoverArgs(args, table.table_)) { NOT_YET("log an error"); }
  return table;
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

  auto expanded_fnargs = ExpandedFnArgs(args);
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
