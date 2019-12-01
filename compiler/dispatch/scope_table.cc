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

// TODO this is independent of scope literals and therefore should be moved back
// into verify_type.cc.
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

ir::Results EmitCallOneOverload(
    Compiler *compiler, ir::Jump const *jump,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  std::vector<ir::Results> arg_results;
  // TODO prep args (if it's a variant, e.g.)
  auto const &params = jump->params();
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

  NOT_YET();
  return ir::Results{};
}

}  // namespace compiler::internal

namespace compiler {

base::expected<ScopeDispatchTable> ScopeDispatchTable::Verify(
    Compiler *compiler, ast::ScopeNode const *node,
    absl::flat_hash_map<ir::Jump const *, ir::ScopeDef const *> inits,
    core::FnArgs<VerifyResult> const &args) {
  absl::flat_hash_map<ir::ScopeDef const *,
                      absl::flat_hash_map<ir::Jump const *, FailedMatch>>
      failures;
  ScopeDispatchTable table;
  table.init_map_ = std::move(inits);
  for (auto[jump, scope] : table.init_map_) {
    auto result = MatchArgsToParams(jump->params(), args);
    if (not result) {
      failures[scope].emplace(jump, result.error());
    } else {
      table.tables_[scope].inits.emplace(jump, *result);
    }
  }

  auto expanded_fnargs = ExpandedFnArgs(args);
  expanded_fnargs.erase(
      std::remove_if(
          expanded_fnargs.begin(), expanded_fnargs.end(),
          [&](core::FnArgs<type::Type const *> const &fnargs) {
            for (auto const & [ scope_def, one_table ] : table.tables_) {
              for (auto const & [ init, params ] : one_table.inits) {
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
  for (auto[scope_def, one_table] : table.tables_) {
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
        bool success =
            one_table.blocks.emplace(&block, std::move(table)).second;
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

ir::Results ScopeDispatchTable::EmitCall(
    Compiler *compiler,
    core::FnArgs<type::Typed<ir::Results>> const &args) const {
  // TODO dispatch test.
  if (init_map_.size() == 1) {
    auto const & [ jump, scope_def ] = *init_map_.begin();
    return internal::EmitCallOneOverload(compiler, jump, args);
  } else {
    NOT_YET();
  }
  return ir::Results{};
}

}  // namespace compiler
