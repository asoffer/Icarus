#include <optional>
#include <utility>

#include "absl/strings/str_format.h"
#include "ast/ast.h"
#include "base/defer.h"
#include "compiler/compiler.h"
#include "ir/compiled_scope.h"
#include "ir/instruction/inliner.h"
#include "ir/value/reg.h"
#include "ir/value/scope.h"
#include "ir/value/value.h"

namespace compiler {
namespace {

// TODO: I don't think we need to return the arguments anymore.
absl::flat_hash_map<
    std::string_view,
    std::pair<ir::BasicBlock *, core::Arguments<type::Typed<ir::Value>>>>
InlineJumpIntoCurrent(ir::Builder &bldr, ir::Jump to_be_inlined,
                      absl::Span<ir::Value const> arguments,
                      ir::LocalBlockInterpretation const &block_interp) {
  auto const *jump           = ir::CompiledJump::From(to_be_inlined);
  auto *start_block          = bldr.CurrentBlock();
  size_t inlined_start_index = bldr.CurrentGroup()->blocks().size();

  auto *into = bldr.CurrentGroup();
  ir::InstructionInliner inl(jump, into, block_interp);

  bldr.CurrentBlock() = start_block;
  size_t i            = 0;
  auto jump_type      = jump->type();
  if (auto state_type = jump_type->state()) {
    type::Apply(state_type, [&]<typename T>()->ir::Reg {
      return bldr.CurrentBlock()->Append(ir::RegisterInstruction<T>{
          .operand = arguments[i++].get<ir::RegOr<T>>(),
          .result  = bldr.CurrentGroup()->Reserve(),
      });
    });
  }
  for (auto const &p : jump_type->params()) {
    type::Apply(p.value, [&]<typename T>()->ir::Reg {
      return bldr.CurrentBlock()->Append(ir::RegisterInstruction<T>{
          .operand = arguments[i++].get<ir::RegOr<T>>(),
          .result  = bldr.CurrentGroup()->Reserve(),
      });
    });
    // TODO Handle types not covered by Apply (structs, etc).
  }

  auto *entry = inl.InlineAllBlocks();

  bldr.CurrentBlock() = start_block;
  bldr.UncondJump(entry);

  return std::move(inl).ExtractNamedBlockMapping();
}

}  // namespace

ir::Value Compiler::EmitValue(ast::ScopeNode const *node) {
  LOG("ScopeNode", "Emitting IR for ScopeNode");

  ASSIGN_OR(return ir::Value(),  //
                   auto scope, EvaluateOrDiagnoseAs<ir::Scope>(node->name()));
  auto const *compiled_scope = ir::CompiledScope::From(scope);
  // Stateful scopes need to have their state initialized.
  std::optional<ir::Reg> state_ptr;
  if (auto state_type = compiled_scope->state_type()) {
    state_ptr = builder().Alloca(state_type);
  }

  // Arguments to the scope's start must be re-evaluated on each call to `goto
  // start()`, so weneed  a block to which we can jump for this purpose.
  auto *args_block =
      builder().AddBlock(absl::StrFormat("args block for scope %p", node));
  auto *landing_block =
      builder().AddBlock(absl::StrFormat("landing block for scope %p", node));

  // TODO: Support blocks evaluating to values.
  add_scope_landing(TransientState::ScopeLandingState{
      .label = node->label() ? node->label()->value() : ir::Label(),
      .block = landing_block,
      .phi   = nullptr,
  });
  base::defer d = [&] { pop_scope_landing(); };

  // Add new blocks
  absl::flat_hash_map<ast::BlockNode const *, ir::BasicBlock *> interp_map;
  for (auto const &block : node->blocks()) {
    interp_map.emplace(&block, builder().AddBlock(absl::StrCat(
                                   "Start of block `", block.name(), "`.")));
  }

  ir::LocalBlockInterpretation local_interp(std::move(interp_map), args_block,
                                            landing_block);

  // Evaluate the arguments on the initial `args_block`.
  builder().UncondJump(args_block);
  builder().CurrentBlock() = args_block;

  // TODO: Support dynamic dispatch.
  auto const &inits = compiled_scope->inits();
  ASSERT(inits.size() == 1u);
  auto &init = *inits.begin();
  // TODO: I'd rather assert that this is true.
  if (ir::CompiledJump::From(init)->work_item) {
    auto f = std::move(*ir::CompiledJump::From(init)->work_item);
    if (f) { std::move(f)(); }
  }

  auto args = node->args().Transform([this](ast::Expression const *expr) {
    return type::Typed<ir::Value>(EmitValue(expr), type_of(expr));
  });

  auto arg_values = PrepareCallArguments(
      ir::CompiledJump::From(init)->type()->state(),
      ir::CompiledJump::From(init)->params().Transform(
          [](auto const &p) { return type::QualType::NonConstant(p.type()); }),
      args);

  auto block_map =
      InlineJumpIntoCurrent(builder(), init, arg_values, local_interp);

  absl::flat_hash_map<std::string_view, std::vector<ir::BasicBlock *>>
      blocks_to_wire;
  for (auto const &[name, block_and_args] : block_map) {
    blocks_to_wire[name].push_back(block_and_args.first);
  }

  for (auto const &block_node : node->blocks()) {
    // It's possible that a block nodes is provably inaccessible. In such cases
    // we do not emit basic blocks for it in `InlineJumpIntoCurrent` and do not
    // need to wire anything up here either.
    auto iter = block_map.find(block_node.name());
    if (iter == block_map.end()) { continue; }

    auto const &[block, args] = iter->second;
    builder().CurrentBlock()  = block;

    auto *start = local_interp[block_node.name()];
    builder().UncondJump(start);

    builder().CurrentBlock() = start;
    auto *b                  = builder().AddBlock(
        absl::StrFormat("body block for `%s`.", block_node.name()));
    builder().UncondJump(b);

    builder().CurrentBlock() = b;
    EmitValue(&block_node);

    // TODO: Get yielded arguments.
    auto const *scope_block =
        ir::CompiledBlock::From(compiled_scope->block(block_node.name()));

    auto const &afters = scope_block->after();
    // TODO: Choose the right jump.
    ASSERT(afters.size() == 1u);
    auto &after = *afters.begin();
    // TODO: I'd rather assert that this is true.
    if (ir::CompiledJump::From(after)->work_item) {
      auto f = std::move(*ir::CompiledJump::From(after)->work_item);
      if (f) { std::move(f)(); }
    }

    auto landing_block_map =
        InlineJumpIntoCurrent(builder(), after, {}, local_interp);
    for (auto const &[name, block_and_args] : landing_block_map) {
      blocks_to_wire[name].push_back(block_and_args.first);
    }
  }

  LOG("ScopeNode", "Blocks to wire up: %s", blocks_to_wire);
  for (const auto &[name, blocks] : blocks_to_wire) {
    auto *landing = local_interp[name];
    for (auto *block : blocks) {
      builder().CurrentBlock() = block;
      builder().UncondJump(landing);
    }
  }

  builder().CurrentBlock() = landing_block;
  return ir::Value();
}

}  // namespace compiler
