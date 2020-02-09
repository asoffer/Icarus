#include "compiler/dispatch/scope_table.h"

#include "ast/expression.h"
#include "base/debug.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "compiler/dispatch/runtime.h"
#include "core/fn_params.h"
#include "diagnostic/errors.h"
#include "ir/builder.h"
#include "ir/components.h"
#include "type/cast.h"
#include "type/type.h"
#include "type/variant.h"

namespace compiler {
namespace {

// Returns ir::OutParams that get passed on to `exit`
std::pair<ir::BasicBlock const *, ir::OutParams> EmitCallOneOverload(
    absl::flat_hash_map<std::string_view,
                        std::pair<ir::BasicBlock *,
                                  core::FnArgs<type::Typed<ir::Results>>>> const
        &name_to_block,
    ir::ScopeDef const *scope_def, Compiler *compiler,
    ir::LocalBlockInterpretation const &block_interp) {
  std::pair<ir::BasicBlock const *, ir::OutParams> exit_outs;
  auto &bldr = compiler->builder();

  for (auto &[next_block_name, block_and_args] : name_to_block) {
    auto &[block, block_args] = block_and_args;
    bldr.CurrentBlock()       = block;
    ir::BlockDef *block_def =
        ASSERT_NOT_NULL(scope_def->block(next_block_name));

    core::FnArgs<type::Type const *> arg_types =
        block_args.Transform([](auto const &arg) { return arg.type(); });

    // TODO make an overload set and call it appropriately.
    // TODO We're calling operator* on an optional. Are we sure that's safe?
    // Did we check it during type-verification? If so why do we need the
    // create_ function in ir::OverloadSet?
    std::optional<ir::AnyFunc> maybe_fn = block_def->before_.Lookup(arg_types);
    ASSERT(maybe_fn.has_value() == true);
    ir::AnyFunc fn = *maybe_fn;
    auto *fn_type  = fn.is_fn() ? fn.func()->type() : fn.foreign().type();

    std::vector<ir::Results> arg_results =
        PrepareCallArguments(compiler, fn_type->params(), block_args);

    ir::OutParams outs = bldr.OutParams(fn_type->output());
    bldr.Call(fn, fn_type, arg_results, outs);

    // TODO only null because there's no start/exit block node. can we fake it
    // to make this work nicer?

    if (next_block_name == "exit") {
      exit_outs.first  = bldr.CurrentBlock();
      exit_outs.second = std::move(outs);

    } else if (auto *block_node = block_interp.block_node(next_block_name)) {
      auto const &params = block_node->params();

      size_t i = 0;
      for (auto &param : params) {
        compiler->EmitMoveInit(
            fn_type->output()[i], ir::Results{outs[i]},
            type::Typed<ir::Reg>(
                compiler->addr(param.value.get()),
                type::Ptr(compiler->type_of(param.value.get()))));
        ++i;
      }
    }

    bldr.UncondJump(block_interp[next_block_name]);
  }

  DEBUG_LOG("EmitCallOneOverload")(*bldr.CurrentGroup());
  return exit_outs;
}

}  // namespace

void internal::OneTable::VerifyBlocks(Compiler *compiler,
                                      ast::ScopeNode const *node) {
  for (auto const &block : node->blocks()) {
    DEBUG_LOG("VerifyBlocks")
    ("Verifying dispatch for block `", block.name(), "`");
    auto const *block_def = scope_def_->block(block.name());
    if (not block_def) { NOT_YET("log an error"); }
    auto block_results = compiler->VerifyBlockNode(&block);
    DEBUG_LOG("VerifyBlocks")("    ", block_results);
    if (block_results.empty()) {
      // There are no relevant yield statements
      DEBUG_LOG("VerifyBlocks")("    ... empty block results");

      ASSIGN_OR(continue,  //
                auto jump_table,
                JumpDispatchTable::Verify(block_def->after_, {}));
      bool success = blocks.emplace(&block, std::move(jump_table)).second;
      static_cast<void>(success);
      ASSERT(success == true);
    } else {
      // Find an `after` that matches
      for (auto const &fn_args : block_results) {
        DEBUG_LOG("VerifyBlocks")("    ... result = ", fn_args);
        ASSIGN_OR(continue,  //
                  auto jump_table,
                  JumpDispatchTable::Verify(block_def->after_, fn_args));
        bool success = blocks.emplace(&block, std::move(jump_table)).second;
        static_cast<void>(success);
        ASSERT(success == true);
      }
    }
    DEBUG_LOG("VerifyBlocks")("    ... done.");
  }
}

void internal::OneTable::VerifyJumps() {
  // The types that get passed out of a `before` function into the next block.
  absl::flat_hash_map<std::string_view,
                      std::vector<absl::Span<type::Type const *const>>>
      next_types;
  for (auto const &[node, table] : blocks) {
    for (auto const &[jump, expr_data] : table.table_) {
      auto jump_exit_paths = jump->ExtractExitPaths();
      for (auto const &[block_name, arg_type_calls] : jump_exit_paths) {
        auto &block_def = *ASSERT_NOT_NULL(scope_def_->block(block_name));
        for (auto const &arg_types : arg_type_calls) {
          std::optional<ir::AnyFunc> maybe_fn =
              block_def.before_.Lookup(arg_types);
          ASSERT(maybe_fn.has_value() == true);
          ir::AnyFunc fn = *maybe_fn;
          core::FnParams<type::Typed<ast::Declaration const *>> fn_params;
          if (fn.is_fn()) {
            fn_params = fn.func()->params();
          } else {
            NOT_YET();
          }

          auto result = MatchArgsToParams(fn_params, arg_types);

          if (result) {
            next_types[block_name].push_back(fn.func()->type()->output());
          } else {
            DEBUG_LOG("VerifyJumps")(result.error());
            // This is entirely reasonable. It just means this particular path
            // into a block can't be used but others are possible.
            NOT_YET();
          }

          // TODO check that ParamsCoverArgs and otherwise emit a diagnostic.
        }
      }
    }
  }

  if (auto iter = next_types.find("exit"); iter != next_types.end()) {
    // Note: If `exit` is not found in `next_types` it's because it is
    // impossible to jump to. This means we'll never exit a scope via a normal
    // call to `exit()` so it's safe to assume that the result type is void
    // (which is correctly default constructed so there's nothing to do).
    result_types_ = type::MultiVar(iter->second);
  }

  // TODO assign types for all the other blocks? Check that they're correct?
}

base::expected<ScopeDispatchTable> ScopeDispatchTable::Verify(
    Compiler *compiler, ast::ScopeNode const *node,
    absl::flat_hash_map<ir::Jump *, ir::ScopeDef const *> inits,
    core::FnArgs<type::QualType> const &args) {
  absl::flat_hash_map<ir::ScopeDef const *,
                      absl::flat_hash_map<ir::Jump *, FailedMatch>>
      failures;
  ScopeDispatchTable table;
  table.scope_node_ = node;
  table.init_map_   = std::move(inits);
  for (auto [jump, scope] : table.init_map_) {
    if (auto result = MatchArgsToParams(jump->params(), args)) {
      auto &one_table = table.tables_[scope];
      one_table.inits.emplace(jump, *result);
      one_table.scope_def_ = scope;
    } else {
      failures[scope].emplace(jump, result.error());
    }
  }

  if (not ParamsCoverArgs(args, table.init_map_,
                          [](ir::Jump *jump, auto const &) -> decltype(auto) {
                            return jump->params();
                          })) {
    compiler->diag().Consume(diagnostic::ParametersDoNotCoverArguments{
        .args = args,
    });
  }

  // If there are any scopes in this overload set that do not have blocks of the
  // corresponding names, we should exit.

  DEBUG_LOG("ScopeNode")("Num tables = ", table.tables_.size());
  for (auto &[_, one_table] : table.tables_) {
    one_table.VerifyBlocks(compiler, node);
    one_table.VerifyJumps();
  }

  std::vector<absl::Span<type::Type const * const>> result_types;
  result_types.reserve(table.tables_.size());
  for (auto const &[_,one_table] : table.tables_) {
    result_types.push_back(one_table.result_types());
  }
  table.qual_type_ =
      type::QualType::NonConstant(type::Tup(type::MultiVar(result_types)));
  return table;
}

ir::Results ScopeDispatchTable::EmitCall(
    Compiler *compiler,
    core::FnArgs<type::Typed<ir::Results>> const &args) const {
  DEBUG_LOG("ScopelDispatchTable")
  ("Emitting a table with ", init_map_.size(), " entries.");
  auto &bldr = compiler->builder();

  std::vector<ir::BasicBlock const *> exit_blocks;
  std::vector<ir::OutParams> exit_outs;

  auto *landing_block  = bldr.AddBlock();
  auto callee_to_block = bldr.AddBlocks(init_map_);
  auto *starting_block = bldr.CurrentBlock();

  // Add basic blocks for each block node in the scope (for each scope
  // definition which might be callable).
  absl::flat_hash_map<ir::ScopeDef const *, ir::LocalBlockInterpretation>
      block_interps;
  for (auto const &[scope_def, _] : tables_) {
    auto [iter, success] = block_interps.emplace(
        scope_def, bldr.MakeLocalBlockInterpretation(
                       scope_node_, starting_block, landing_block));
    static_cast<void>(success);
    ASSERT(success == true);
  }

  EmitRuntimeDispatch(bldr, init_map_, callee_to_block, args);

  for (auto const &[jump, scope_def] : init_map_) {
    bldr.CurrentBlock() = callee_to_block[jump];
    // Argument preparation is done inside EmitCallOneOverload

    auto name_to_block = JumpDispatchTable::EmitCallOneOverload(
        jump, compiler, args, block_interps.at(scope_def));
    auto [block, outs] = EmitCallOneOverload(name_to_block, scope_def, compiler,
                                             block_interps.at(scope_def));
    if (not outs.empty()) {
      exit_blocks.push_back(block);
      exit_outs.push_back(std::move(outs));
    }
  }
  bldr.CurrentBlock() = landing_block;

  // TODO handle results

  auto state = bldr.block_termination_state();
  for (auto const &[scope_def, one_table] : tables_) {
    for (auto const &[node, table] : one_table.blocks) {
      DEBUG_LOG("EmitCall")(node->DebugString());

      bldr.CurrentBlock() = block_interps.at(scope_def)[node];
      bldr.block_termination_state() =
          ir::Builder::BlockTerminationState::kMoreStatements;
      auto yield_args = compiler->EmitBlockNode(node);

      // TODO skipping after-handlers is incorrect. We need a guaranteed
      // exit path.
      if (bldr.block_termination_state() ==
          ir::Builder::BlockTerminationState::kReturn) {
        continue;
      }

      auto yield_typed_results = yield_args.Transform(
          [](auto const &p) { return type::Typed(p.first, p.second.type()); });

      auto callee_to_block = bldr.AddBlocks(table.table_);
      EmitRuntimeDispatch(bldr, table.table_, callee_to_block,
                          yield_typed_results);

      for (auto const &[jump, expr_data] : table.table_) {
        bldr.CurrentBlock() = callee_to_block[jump];

        auto name_to_block = JumpDispatchTable::EmitCallOneOverload(
            jump, compiler, yield_typed_results, block_interps.at(scope_def));
        auto [block, outs] = EmitCallOneOverload(
            name_to_block, scope_def, compiler, block_interps.at(scope_def));
        if (not outs.empty()) {
          exit_blocks.push_back(block);
          exit_outs.push_back(std::move(outs));
        }
      }
    }
  }

  bldr.block_termination_state() = state;
  bldr.CurrentBlock()            = landing_block;
  DEBUG_LOG("EmitCall")(*bldr.CurrentGroup());
  switch (exit_outs.size()) {
    case 0: return ir::Results{};
    case 1: {
      ir::Results results;
      // TODO direct outparams -> results conversion?
      auto &out_params = exit_outs[0];
      for (size_t i = 0; i < out_params.size(); ++i) {
        results.append(out_params[i]);
      }
      return results;
    }
    default: {
      std::vector<ir::RegOr<int64_t>> values;
      values.reserve(exit_blocks.size());
      for (auto &out_params : exit_outs) { values.emplace_back(out_params[0]); }
      return ir::Results{bldr.Phi(exit_blocks, std::move(values))};
    }
  }
}

}  // namespace compiler
