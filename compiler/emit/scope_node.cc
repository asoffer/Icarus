#include <optional>
#include <queue>
#include <string_view>
#include <type_traits>
#include <utility>

#include "absl/cleanup/cleanup.h"
#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/strings/str_format.h"
#include "absl/types/span.h"
#include "ast/ast.h"
#include "base/debug.h"
#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "base/meta.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/blocks/basic.h"
#include "ir/blocks/group.h"
#include "ir/compiled_scope.h"
#include "ir/instruction/core.h"
#include "ir/instruction/jump.h"
#include "ir/value/char.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/scope.h"

namespace compiler {
namespace {

// Many different gotos may end up at the same block node, some from the same
// jump, some from different jumps. They may end up calling different overloads
// of the before/entry function. BeforeBlock describes one such possible entry
// path.
struct BeforeBlock : base::Extend<BeforeBlock>::With<base::AbslHashExtension> {
  ast::BlockNode const *block;
  ir::Fn fn;
};


struct BlockMetadata {
  ast::BlockNode const *block_node;
  ir::BasicBlock *block;
};

void EmitIrForBlockNode(Compiler &c, ast::BlockNode const *node,
                        ir::BasicBlock *body) {
  auto &bldr          = c.builder();
  bldr.CurrentBlock() = body;

  for (auto const &decl : node->params()) {
    auto const *param = decl.value.get();
    auto addr = c.builder().Alloca(c.context().qual_types(param)[0].type());
    // TODO: Support multiple declarations?
    c.builder().set_addr(&param->ids()[0], addr);
  }

  bldr.block_termination_state() =
      ir::Builder::BlockTerminationState::kMoreStatements;
  ir::PartialResultBuffer buffer;
  c.EmitToBuffer(node, buffer);
}

std::optional<ir::Reg> InitializeScopeState(Compiler &c,
                                            ir::CompiledScope const &scope) {
  if (type::Type state_type = scope.state_type()) {
    ir::Reg state_ptr = c.builder().Alloca(state_type);
    c.EmitDefaultInit(type::Typed<ir::Reg>(state_ptr, state_type));
    return state_ptr;
  }
  return std::nullopt;
}

std::pair<ir::Jump, ir::PartialResultBuffer> EmitIrForJumpArguments(
    Compiler &c, ir::PartialResultBuffer const &constant_arguments,
    std::optional<ir::Reg> state_ptr,
    absl::Span<ast::Call::Argument const> const &args,
    ir::CompiledScope const &scope) {
  // TODO: Support dynamic dispatch.
  auto const &inits = scope.inits();
  ASSERT(inits.size() == 1u);
  auto &init = *inits.begin();

  // Arguments provided to a function call need to be "prepared" in the sense
  // that they need to be
  // * Ordered according to the parameters of the function (because named
  //   arguments may be out of order)
  // * Have any implicit conversions applied.
  //
  // Implicit conversions are tricky because we cannot first compute the values
  // and then apply conversions to them. This may work for conversions that take
  // a buffer-pointer and convert it to just a pointer, but some conversions
  // take values and convert them to pointers/references. If we first compute
  // the value, we may end up loading the value from memory and no longer having
  // access to its address. Or worse, we may have a temporary and never have an
  // allocated address for it.
  ir::PartialResultBuffer prepared_arguments;

  if (state_ptr) { prepared_arguments.append(*state_ptr); }

  auto const &params = ir::CompiledJump::From(init)->params();
  size_t i = 0;

  // TODO: Some of them could be constant.
  auto const &param_qts = ir::CompiledJump::From(init)->params().Transform(
      [](auto const &p) { return type::QualType::NonConstant(p.type()); });

  for (auto const &argument : args) {
    absl::Cleanup cleanup = [&] { ++i; };
    // TODO: Default arguments.

    PrepareArgument(c, constant_arguments[i], &argument.expr(),
                    param_qts[i].value, prepared_arguments);
  }

  return std::make_pair(init, std::move(prepared_arguments));
}

void InlineStartIntoCurrent(Compiler &c, ir::Scope scope,
                            ir::BasicBlock *entry_block,
                            ir::BasicBlock *starting_block,
                            absl::Span<ast::Call::Argument const> arguments) {
  c.builder().CurrentBlock() = entry_block;
  auto const *compiled_scope = ir::CompiledScope::From(scope);

  // If the scope is stateful, stack-allocate space for the state and
  // default-initialize it.
  //
  // TODO: This interface isn't great beacuse it requires default-initializable
  // state and no way to call any other initializer.
  std::optional<ir::Reg> state_ptr = InitializeScopeState(c, *compiled_scope);

  c.builder().UncondJump(starting_block);
  c.builder().CurrentBlock() = starting_block;

  auto constant_args = EmitConstantPartialResultBuffer(c, arguments);
  auto [init, args]  = EmitIrForJumpArguments(c, constant_args, state_ptr,
                                             arguments, *compiled_scope);
  c.builder().InlineJumpIntoCurrent(init, args,
                                    c.state().scope_landings.back().names);
}

}  // namespace

void Compiler::EmitToBuffer(ast::ScopeNode const *node,
                            ir::PartialResultBuffer &out) {
  LOG("ScopeNode", "Emitting IR for ScopeNode");
  ir::Scope scope = *EvaluateOrDiagnoseAs<ir::Scope>(node->name());

  auto *entry_block = builder().CurrentBlock();

  // Emit IR for each block node, emission for each block node handles jumping
  // to the correct location, including jumps as well as subsequent calls to
  // before. This means that we have to ensure the body blocks already exist.
  absl::flat_hash_map<std::string_view, BlockMetadata> blocks_by_name;
  absl::flat_hash_map<std::string_view, ir::BasicBlock *> names;

  // Arguments to the scope's start must be re-evaluated on each call to `goto
  // start()`, so we need a block to which we can jump for this purpose.
  auto *starting_block = builder().AddBlock("scope-start");
  blocks_by_name.emplace(
      "start", BlockMetadata{.block_node = nullptr, .block = starting_block});
  names.emplace("start", starting_block);

  // If a scope has a path that exits normally along some path (a call to exit
  // rather than a return or labelled yield), then this is the block on which we
  // land.
  auto *landing_block = builder().AddBlock("scope-done");
  blocks_by_name.emplace(
      "done", BlockMetadata{.block_node = nullptr, .block = landing_block});
  names.emplace("done", landing_block);

  absl::Cleanup c = [&] { state().scope_landings.pop_back(); };

  for (auto const &block_node : node->blocks()) {
    auto *b = builder().AddBlock(
        absl::StrFormat("Body of block `%s`.", block_node.name()));
    blocks_by_name.emplace(
        block_node.name(),
        BlockMetadata{.block_node = &block_node, .block = b});
    names.emplace(block_node.name(), b);
    // TODO: Add Phi nodes onto each of these blocks.
  }

  // Push the scope landing state onto the the vector. Any nested scopes will be
  // able to lookup the label in this vector to determine where they should jump
  // to.
  state().scope_landings.push_back(TransientState::ScopeState{
      .label = node->label() ? node->label()->value() : ir::Label(),
      .scope = scope,
      // TODO: Implement me
      .result_type = type::QualType::NonConstant(type::Void),
      .block       = landing_block,
      .names       = std::move(std::move(names)),
  });

  for (auto [name, metadata] : blocks_by_name) {
    if (not metadata.block_node) { continue; }
    EmitIrForBlockNode(*this, metadata.block_node, metadata.block);
  }

  InlineStartIntoCurrent(*this, scope, entry_block, starting_block, node->arguments());

  builder().CurrentBlock() = landing_block;
  builder().block_termination_state() =
      ir::Builder::BlockTerminationState::kMoreStatements;
  // TODO: Do you need to add a phi node here?
}

void Compiler::EmitCopyInit(
    ast::ScopeNode const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  // TODO: Implement this properly.
  EmitCopyAssign(node, to);
}

void Compiler::EmitMoveInit(
    ast::ScopeNode const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  // TODO: Implement this properly.
  EmitMoveAssign(node, to);
}
void Compiler::EmitCopyAssign(
    ast::ScopeNode const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  // TODO: Implement this properly.
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyAssign(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveAssign(
    ast::ScopeNode const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  // TODO: Implement this properly.
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveAssign(to[0], type::Typed(buffer[0], t));
}

}  // namespace compiler
