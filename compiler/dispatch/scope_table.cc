#include "compiler/dispatch/scope_table.h"

#include "ast/expression.h"
#include "base/debug.h"
#include "compiler/compiler.h"
#include "compiler/dispatch/parameters_and_arguments.h"
#include "compiler/extract_jumps.h"
#include "core/fn_params.h"
#include "ir/builder.h"
#include "ir/components.h"
#include "ir/inliner.h"
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

std::pair<std::vector<type::Type const *>, std::vector<ir::Results>>
ExtractArgsAndTypes(core::FnArgs<type::Typed<ir::Results>> const &args) {
  std::vector<type::Type const *> arg_types;
  std::vector<ir::Results> arg_results;
  arg_types.reserve(args.size());
  arg_results.reserve(args.size());
  // TODO this is the wrong linearization. We have tools for this with FnArgs
  // and FnParams.
  args.Apply([&](type::Typed<ir::Results> const &arg) {
    arg_types.push_back(arg.type());
    arg_results.push_back(arg.get());
  });
  return std::pair(std::move(arg_types), std::move(arg_results));
}

// TODO organize the parameters here, they're getting to be too much.
void EmitCallOneOverload(ir::ScopeDef const *scope_def,
                         ir::BasicBlock *starting_block,
                         ir::BasicBlock *landing_block, Compiler *compiler,
                         ir::Jump *jump,
                         core::FnArgs<type::Typed<ir::Results>> const &args,
                         ir::LocalBlockInterpretation const &block_interp) {
  auto arg_results = PrepareCallArguments(compiler, jump->params(), args);
  static_cast<void>(arg_results);
  // TODO pass arguments to inliner.

  auto &bldr         = compiler->builder();
  auto name_to_block = ir::Inline(bldr, jump, arg_results, block_interp);

  for (auto &[next_block_name, block_and_args] : name_to_block) {
    auto &[block, block_args] = block_and_args;
    bldr.CurrentBlock()       = block;
    if (next_block_name == "start") {
      bldr.UncondJump(starting_block);
    } else if (next_block_name == "exit") {
      auto [done_types, done_results]     = ExtractArgsAndTypes(block_args);
      std::optional<ir::AnyFunc> maybe_fn = scope_def->dones_[done_types];
      ASSERT(maybe_fn.has_value() == true);
      ir::AnyFunc fn = *maybe_fn;
      auto *fn_type  = fn.is_fn() ? fn.func()->type() : fn.foreign().type();
      bldr.Call(fn, fn_type, done_results);
      bldr.UncondJump(landing_block);
    } else {
      ir::BlockDef *block_def =
          ASSERT_NOT_NULL(scope_def->block(next_block_name));

      // TODO make an overload set and call it appropriately.
      // TODO We're calling operator* on an optional. Are we sure that's safe?
      // Did we check it during type-verification? If so why do we need the
      // create_ function in ir::OverloadSet?
      auto [arg_types, arg_results]       = ExtractArgsAndTypes(block_args);
      std::optional<ir::AnyFunc> maybe_fn = block_def->before_[arg_types];
      ASSERT(maybe_fn.has_value() == true);
      ir::AnyFunc fn = *maybe_fn;
      auto *fn_type  = fn.is_fn() ? fn.func()->type() : fn.foreign().type();
      bldr.Call(fn, fn_type, arg_results);
      bldr.UncondJump(block_interp[next_block_name]);
    }
  }

  bldr.CurrentBlock() = landing_block;
  DEBUG_LOG("EmitCallOneOverload")(*bldr.CurrentGroup());
}

// Emits code which determines if a function with parameters `params` should be
// called with arguments `args`. It does this by looking for variants in `args`
// and testing the actually held type to see if it matches the corresponding
// parameter type. Note that the parameter type need not be identical. Rather,
// there must be a cast from the actual argument type to the parameter type
// (usually due to a cast such as `int64` casting to `int64 | bool`).
ir::RegOr<bool> EmitRuntimeDispatchOneComparison(
    ir::Builder &bldr,
    core::FnParams<type::Typed<ast::Declaration const *>> const &params,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  size_t i = 0;
  for (; i < args.pos().size(); ++i) {
    auto &arg     = args.pos()[i];
    auto *arg_var = arg.type()->if_as<type::Variant>();
    if (not arg_var) { continue; }
    auto runtime_type =
        ir::Load<type::Type const *>(bldr.VariantType(arg->get<ir::Addr>(0)));
    // TODO Equality isn't the right thing to check
    return bldr.Eq(runtime_type, params.at(i).value.type());
  }
  for (; i < params.size(); ++i) {
    auto const &param = params.at(i);
    auto *arg         = args.at_or_null(param.name);
    if (not arg) { continue; }  // Default arguments
    auto *arg_var = arg->type()->if_as<type::Variant>();
    if (not arg_var) { continue; }
    NOT_YET();
  }
  return ir::RegOr<bool>(false);
}

void EmitRuntimeDispatch(
    ir::Builder &bldr,
    absl::flat_hash_map<ir::Jump *, ir::ScopeDef const *> const &table,
    absl::flat_hash_map<ir::Jump *, ir::BasicBlock *> const &callee_to_block,
    core::FnArgs<type::Typed<ir::Results>> const &args) {
  // TODO This is a simple linear search through the table which is certainly a
  // bad idea. We can optimize it later. Likely the right way to do this is to
  // find a perfect hash of the function variants that produces an index into a
  // block table so we pay for a hash and a single indirect jump. This may be
  // harder if you remove variant and implement `overlay`.

  auto iter = table.begin();

  while (true) {
    auto const &[jump, scope_def] = *iter;
    ++iter;

    if (iter == table.end()) {
      bldr.UncondJump(callee_to_block.at(jump));
      break;
    }

    ir::RegOr<bool> match =
        EmitRuntimeDispatchOneComparison(bldr, jump->params(), args);
    bldr.CurrentBlock() =
        ir::EarlyExitOn<true>(callee_to_block.at(jump), match);
  }
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
    NOT_YET("log an error");
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
  DEBUG_LOG("ScopelDispatchTable")
  ("Emitting a table with ", init_map_.size(), " entries.");
  auto &bldr = compiler->builder();

  auto *landing_block  = bldr.AddBlock();
  auto callee_to_block = bldr.AddBlocks(init_map_);

  // Add basic blocks for each block node in the scope (for each scope
  // definition which might be callable).
  absl::flat_hash_map<ir::ScopeDef const *, ir::LocalBlockInterpretation>
      block_interps;
  for (auto const &[scope_def, _] : tables_) {
    auto [iter, success] = block_interps.emplace(
        scope_def, bldr.MakeLocalBlockInterpretation(scope_node_));
    static_cast<void>(success);
    ASSERT(success == true);
  }

  auto *starting_block = bldr.CurrentBlock();
  EmitRuntimeDispatch(bldr, init_map_, callee_to_block, args);

  for (auto const &[jump, scope_def] : init_map_) {
    bldr.CurrentBlock() = callee_to_block[jump];
    // Argument preparation is done inside EmitCallOneOverload
    EmitCallOneOverload(scope_def, starting_block, landing_block, compiler,
                        jump, args, block_interps.at(scope_def));
  }

  // TODO handle results

  bool more = bldr.more_stmts_allowed();
  for (auto const &[scope_def, one_table] : tables_) {
    for (auto const &[node, table] : one_table.blocks) {
      DEBUG_LOG("EmitCall")(node->DebugString());
      bldr.CurrentBlock() = block_interps.at(scope_def)[node];
      bldr.allow_more_stmts();
      compiler->Visit(node, EmitValueTag{});

      // TODO unconditionally skipping after-handlers is incorrect.
      if (not bldr.more_stmts_allowed()) { continue; }

      // TODO This is a simplification which only handles the situation where
      // we jump to the after handler that has no arguments.

      ir::BlockDef const *block_def = scope_def->block(node->name());
      EmitCallOneOverload(scope_def, starting_block, landing_block, compiler,
                          block_def->after_[0], {},
                          block_interps.at(scope_def));
    }
  }
  if (more) { bldr.allow_more_stmts(); }
  bldr.CurrentBlock() = landing_block;
  DEBUG_LOG("EmitCall")(*bldr.CurrentGroup());
  return ir::Results{};
}

}  // namespace compiler
