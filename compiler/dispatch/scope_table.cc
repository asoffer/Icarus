#include "compiler/dispatch/scope_table.h"

#include "ast/expression.h"
#include "base/debug.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "compiler/dispatch/runtime.h"
#include "compiler/extract_jumps.h"
#include "core/fn_params.h"
#include "diagnostic/errors.h"
#include "ir/builder.h"
#include "ir/components.h"
#include "type/cast.h"
#include "type/type.h"
#include "type/variant.h"

namespace compiler {
namespace {

// TODO this is independent of scope literals and therefore should be moved back
// into verify_type.cc.
std::vector<core::FnArgs<type::QualType>> VerifyBlockNode(
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
  std::vector<core::FnArgs<type::QualType>> result;
  for (auto *yield : yields) {
    auto &back = result.emplace_back();
    // TODO actually fill a fnargs
    std::vector<std::pair<ast::Expression const *, type::QualType>>
        local_pos_yields;
    for (auto *yield_expr : yields[0]->as<ast::YieldStmt>().exprs()) {
      back.pos_emplace(*ASSERT_NOT_NULL(compiler->qual_type_of(yield_expr)));
    }
  }
  return result;
}

// TODO this doesn't entirely make sense. We use the parameters to pick out the
// correct overload set member, but really that should be done with arguments?
// Something is fishy here.
std::pair<core::FnParams<type::Type const *>, std::vector<ir::Results>>
ExtractArgsAndParams(core::FnArgs<type::Typed<ir::Results>> const &args) {
  core::FnParams<type::Type const *> arg_params;
  std::vector<ir::Results> arg_results;
  arg_results.reserve(args.size());
  // TODO this is the wrong linearization. We have tools for this with FnArgs
  // and FnParams.
  args.Apply([&](type::Typed<ir::Results> const &arg) {
    arg_params.append("", arg.type());
    arg_results.push_back(arg.get());
  });
  return std::pair(std::move(arg_params), std::move(arg_results));
}

// TODO organize the parameters here, they're getting to be too much.
void EmitCallOneOverload(
    absl::flat_hash_map<std::string_view,
                        std::pair<ir::BasicBlock *,
                                  core::FnArgs<type::Typed<ir::Results>>>> const
        &name_to_block,
    ir::ScopeDef const *scope_def, Compiler *compiler,
    ir::LocalBlockInterpretation const &block_interp) {
  auto &bldr = compiler->builder();

  for (auto &[next_block_name, block_and_args] : name_to_block) {
    auto &[block, block_args] = block_and_args;
    bldr.CurrentBlock()       = block;
    ir::BlockDef *block_def =
        ASSERT_NOT_NULL(scope_def->block(next_block_name));

    // TODO make an overload set and call it appropriately.
    // TODO We're calling operator* on an optional. Are we sure that's safe?
    // Did we check it during type-verification? If so why do we need the
    // create_ function in ir::OverloadSet?
    auto [arg_params, arg_results]      = ExtractArgsAndParams(block_args);
    std::optional<ir::AnyFunc> maybe_fn = block_def->before_[arg_params];
    ASSERT(maybe_fn.has_value() == true);
    ir::AnyFunc fn = *maybe_fn;
    auto *fn_type  = fn.is_fn() ? fn.func()->type() : fn.foreign().type();

    ir::OutParams outs = bldr.OutParams(fn_type->output());
    bldr.Call(fn, fn_type, arg_results, outs);

    // TODO only null because there's no start/exit block node. can we fake it
    // to make this work nicer?
    if (auto *block_node = block_interp.block_node(next_block_name)) {
      auto const &params = block_node->params();

      size_t i = 0;
      for (auto *param : params) {
        // TODO should be a decl already
        ir::Reg addr = compiler->addr(&param->as<ast::Declaration>());
        type::Type const *param_type =
            compiler->type_of(&param->as<ast::Declaration>());

        compiler->EmitMoveInit(
            fn_type->output()[i], ir::Results{outs[i]},
            type::Typed<ir::Reg>(addr, type::Ptr(param_type)));
        ++i;
      }
    }

    bldr.UncondJump(block_interp[next_block_name]);
  }

  DEBUG_LOG("EmitCallOneOverload")(*bldr.CurrentGroup());
}

}  // namespace

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
    auto result = MatchArgsToParams(jump->params(), args);
    if (not result) {
      failures[scope].emplace(jump, result.error());
    } else {
      table.tables_[scope].inits.emplace(jump, *result);
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
  for (auto &[scope_def, one_table] : table.tables_) {
    for (auto const &block : node->blocks()) {
      DEBUG_LOG("ScopeNode")
      ("Verifying dispatch for block `", block.name(), "`");
      auto const *block_def = scope_def->block(block.name());
      if (not block_def) { NOT_YET("log an error"); }
      auto block_results = VerifyBlockNode(compiler, &block);
      DEBUG_LOG("ScopeNode")("    ", block_results);
      if (block_results.empty()) {
        // There are no relevant yield statements
        DEBUG_LOG("ScopeNode")("    ... empty block results");

        ASSIGN_OR(continue,  //
                  auto jump_table,
                  JumpDispatchTable::Verify(node, block_def->after_, {}));
        bool success =
            one_table.blocks.emplace(&block, std::move(jump_table)).second;
        static_cast<void>(success);
        ASSERT(success == true);
      } else {
        // Find an `after` that matches
        for (auto const &fn_args : block_results) {
          DEBUG_LOG("ScopeNode")("    ... result = ", fn_args);
          ASSIGN_OR(continue,  //
                    auto jump_table,
                    JumpDispatchTable::Verify(node, block_def->after_, args));

          bool success =
              one_table.blocks.emplace(&block, std::move(jump_table)).second;
          static_cast<void>(success);
          ASSERT(success == true);
        }
      }
      DEBUG_LOG("ScopeNode")("    ... done.");
    }
  }

  return table;
}

ir::Results ScopeDispatchTable::EmitCall(
    Compiler *compiler,
    core::FnArgs<type::Typed<ir::Results>> const &args) const {
  DEBUG_LOG("ScopelDispatchTable")
  ("Emitting a table with ", init_map_.size(), " entries.");
  auto &bldr = compiler->builder();

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
    EmitCallOneOverload(name_to_block, scope_def, compiler,
                        block_interps.at(scope_def));
    bldr.CurrentBlock() = landing_block;
  }

  // TODO handle results

  auto state = bldr.block_termination_state();
  for (auto const &[scope_def, one_table] : tables_) {
    for (auto const &[node, table] : one_table.blocks) {
      DEBUG_LOG("EmitCall")(node->DebugString());
      bldr.CurrentBlock() = block_interps.at(scope_def)[node];
      bldr.block_termination_state() =
          ir::Builder::BlockTerminationState::kMoreStatements;
      auto yield_args = compiler->EmitBlockNode(node);

      // TODO skipping after-handlers is incorrect. We need a guaranteed exit path.
      if (bldr.block_termination_state() ==
          ir::Builder::BlockTerminationState::kReturn) {
        continue;
      }

      ir::BlockDef const *block_def = scope_def->block(node->name());

      // TODO call the appropriate overload set. This logic goes in jump table.
      ir::Jump *chosen = nullptr;
      for (ir::Jump *j : block_def->after_) {
        std::vector<std::pair<ir::Jump *, int>> local_table_hack;
        local_table_hack.emplace_back(j, 0);
        if (ParamsCoverArgs(
                yield_args.Transform([](auto const &p) { return p.second; }),
                local_table_hack,
                [](ir::Jump *jump, auto const &) -> decltype(auto) {
                  return jump->params();
                })) {
          chosen = j;
          break;
        }
      }
      ASSERT(chosen != nullptr);

      auto name_to_block = JumpDispatchTable::EmitCallOneOverload(
          chosen, compiler, yield_args.Transform([](auto const &p) {
            return type::Typed(p.first, p.second.type());
          }),
          block_interps.at(scope_def));
      EmitCallOneOverload(name_to_block, scope_def, compiler,
                          block_interps.at(scope_def));
      bldr.CurrentBlock() = landing_block;
    }
  }

  bldr.block_termination_state() = state;
  bldr.CurrentBlock()            = landing_block;
  DEBUG_LOG("EmitCall")(*bldr.CurrentGroup());
  return ir::Results{};
}

}  // namespace compiler
