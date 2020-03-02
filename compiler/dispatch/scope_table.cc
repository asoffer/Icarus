#include "compiler/dispatch/scope_table.h"

#include "ast/expression.h"
#include "base/debug.h"
#include "base/defer.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "compiler/dispatch/runtime.h"
#include "core/params.h"
#include "diagnostic/errors.h"
#include "ir/builder.h"
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

    // TODO extract qualifiers correctly. here.
    core::FnArgs<type::QualType> arg_types =
        block_args.Transform([](auto const &arg) {
          return type::QualType::NonConstant(arg.type());
        });

    // TODO make an overload set and call it appropriately.
    // TODO We're calling operator* on an optional. Are we sure that's safe?
    // Did we check it during type-verification? If so why do we need the
    // create_ function in ir::OverloadSet?
    std::optional<ir::Fn> maybe_fn = block_def->before_.Lookup(arg_types);
    ASSERT(maybe_fn.has_value() == true);
    ir::Fn fn     = *maybe_fn;
    auto *fn_type = fn.type();

    std::vector<ir::Results> arg_results =
        PrepareCallArguments(compiler, nullptr, fn_type->params(), block_args);

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
                JumpDispatchTable::Verify(scope_def_->state_type_,
                                          block_def->after_, {}));
      bool success = blocks.emplace(&block, std::move(jump_table)).second;
      static_cast<void>(success);
      ASSERT(success == true);
    } else {
      // Find an `after` that matches
      for (auto const &fn_args : block_results) {
        DEBUG_LOG("VerifyBlocks")("    ... result = ", fn_args);
        ASSIGN_OR(continue,  //
                  auto jump_table,
                  JumpDispatchTable::Verify(scope_def_->state_type_,
                                            block_def->after_, fn_args));
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
          std::optional<ir::Fn> maybe_fn = block_def.before_.Lookup(arg_types);
          ASSERT(maybe_fn.has_value() == true);
          ir::Fn fn = *maybe_fn;
          switch (fn.kind()) {
            case ir::Fn::Kind::Native: {
              auto result = MatchArgsToParams(fn.native()->params(), arg_types);

              if (result) {
                next_types[block_name].push_back(fn.native()->type()->output());
              } else {
                DEBUG_LOG("VerifyJumps")(result.error());
                // This is entirely reasonable. It just means this particular
                // path into a block can't be used but others are possible.
                NOT_YET();
              }

              // TODO check that ParamsCoverArgs and otherwise emit a
              // diagnostic.

            } break;
            case ir::Fn::Kind::Builtin: {
              NOT_YET();
            } break;
            case ir::Fn::Kind::Foreign: {
              NOT_YET();
            } break;
          }
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

  if (not ParamsCoverArgs(
          args, table.init_map_,
          [](ir::Jump *jump, auto const &) { return jump->params(); })) {
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

void ScopeDispatchTable::EmitSplittingDispatch(
    Compiler *compiler,
    absl::flat_hash_map<ir::ScopeDef const *, ir::Reg> const &state_regs,
    absl::flat_hash_map<ir::ScopeDef const *,
                        ir::LocalBlockInterpretation> const &block_interps,
    core::FnArgs<type::Typed<ir::Results>> const &args) const {
  auto &bldr           = compiler->builder();
  auto callee_to_block = bldr.AddBlocks(init_map_);
  EmitRuntimeDispatch(bldr, init_map_, callee_to_block, args);

  for (auto const &[jump, scope_def] : init_map_) {
    auto const &block_interp = block_interps.find(scope_def)->second;
    bldr.CurrentBlock() = callee_to_block[jump];
    // Argument preparation is done inside EmitCallOneOverload

    std::optional<ir::Reg> state_reg;
    if (auto iter = state_regs.find(scope_def); iter != state_regs.end()) {
      state_reg = iter->second;
    }
    auto name_to_block = JumpDispatchTable::EmitCallOneOverload(
        state_reg, jump, compiler, args, block_interp);
    auto [block, outs] =
        EmitCallOneOverload(name_to_block, scope_def, compiler, block_interp);
    if (not outs.empty()) {
      // TODO I think we need a phi-node per scope and then to combine them.
      // exit_blocks.push_back(block);
      // exit_outs.push_back(std::move(outs));
      // ASSERT_NOT_NULL(land_phi)->add(block, 1);
      NOT_YET();
    }
  }
  bldr.CurrentBlock() = std::get<1>(compiler->scope_landings().back());
}

void internal::OneTable::EmitCall(
    Compiler *compiler, ir::ScopeDef const *scope_def,
    std::optional<ir::Reg> state_reg,
    ir::LocalBlockInterpretation const &block_interp) const {
  for (auto const &[node, table] : blocks) {
    auto &bldr = compiler->builder();
    DEBUG_LOG("EmitCall")(node->DebugString());

    bldr.CurrentBlock() = block_interp[node];
    bldr.block_termination_state() =
        ir::Builder::BlockTerminationState::kMoreStatements;
    auto yield_results       = compiler->EmitBlockNode(node);
    auto yield_typed_results = yield_results.vals.Transform(
        [](auto const &p) { return type::Typed(p.first, p.second.type()); });

    // TODO skipping after-handlers is incorrect. We need a guaranteed
    // exit path.
    auto scope_landings_span = compiler->scope_landings();
    switch (bldr.block_termination_state()) {
      case ir::Builder::BlockTerminationState::kLabeledYield: {
        for (auto iter = scope_landings_span.rbegin();
             iter != scope_landings_span.rend(); ++iter) {
          auto &[label, scope_landing_block, phi_inst] = *iter;
          if (label == yield_results.label) {
            bldr.UncondJump(scope_landing_block);
            phi_inst->add(bldr.CurrentBlock(), 17);
            break;
            // Update phi-node?!
          } else {
            // TODO call skip.
          }
        }
        continue;
      }
      case ir::Builder::BlockTerminationState::kReturn: {
        for (auto iter = scope_landings_span.rbegin();
             iter != scope_landings_span.rend(); ++iter) {
          // TODO call skip
        }
        continue;
        default: break;
      }
    }

    auto callee_to_block = bldr.AddBlocks(table.table_);
    EmitRuntimeDispatch(bldr, table.table_, callee_to_block,
                        yield_typed_results);

    for (auto const &[jump, expr_data] : table.table_) {
      bldr.CurrentBlock() = callee_to_block[jump];

      auto name_to_block = JumpDispatchTable::EmitCallOneOverload(
          state_reg, jump, compiler, yield_typed_results, block_interp);
      auto [block, outs] =
          EmitCallOneOverload(name_to_block, scope_def, compiler, block_interp);
      if (not outs.empty()) {
        // TODO
        // exit_blocks.push_back(block);
        // exit_outs.push_back(std::move(outs));
        // ASSERT_NOT_NULL(land_phi)->add(block, 1);
        NOT_YET();
      }
    }
  }
}

ir::Results ScopeDispatchTable::EmitCall(
    Compiler *compiler,
    core::FnArgs<type::Typed<ir::Results>> const &args) const {
  DEBUG_LOG("ScopeDispatchTable")
  ("Emitting a table with ", init_map_.size(), " entries.");
  auto &bldr = compiler->builder();

  auto *landing_block  = bldr.AddBlock();
  auto *starting_block = bldr.CurrentBlock();

  ir::PhiInstruction<int64_t> *land_phi = nullptr;
  if (compiler->type_of(scope_node_) != type::Void()) {
    bldr.CurrentBlock() = landing_block;
    land_phi            = bldr.PhiInst<int64_t>();
    bldr.CurrentBlock() = starting_block;
  }
  compiler->add_scope_landing(
      scope_node_->label() ? scope_node_->label()->value() : ir::Label(),
      landing_block, land_phi);
  base::defer d = [&] { compiler->pop_scope_landing(); };

  // Add basic blocks for each block node in the scope (for each scope
  // definition which might be callable).
  absl::flat_hash_map<ir::ScopeDef const *, ir::LocalBlockInterpretation>
      block_interps;

  absl::flat_hash_map<ir::ScopeDef const *, ir::Reg> state_regs;

  for (auto const &[scope_def, one_table] : tables_) {
    auto [iter, success] = block_interps.emplace(
        scope_def, bldr.MakeLocalBlockInterpretation(
                       scope_node_, starting_block, landing_block));
    static_cast<void>(success);
    ASSERT(success == true);

    if (scope_def->state_type_) {
      state_regs[scope_def] =
          compiler->builder().TmpAlloca(scope_def->state_type_);
    }
  }

  EmitSplittingDispatch(compiler, state_regs, block_interps, args);

  auto state = bldr.block_termination_state();
  for (auto const &[scope_def, one_table] : tables_) {
    one_table.EmitCall(compiler, scope_def,
                       scope_def->state_type_
                           ? std::optional(state_regs.find(scope_def)->second)
                           : std::nullopt,
                       block_interps.find(scope_def)->second);
  }

  bldr.block_termination_state() = state;
  bldr.CurrentBlock()            = landing_block;
  DEBUG_LOG("EmitCall")(*bldr.CurrentGroup());
  if (land_phi) {
    return ir::Results{land_phi->result};
  } else {
    return ir::Results{};
  }
}

}  // namespace compiler
