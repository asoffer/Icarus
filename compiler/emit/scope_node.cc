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

struct BlockMetadata {
  ast::BlockNode const *block_node;
  ir::BasicBlock *block;
};

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
    Compiler &c, std::optional<ir::Reg> state_ptr,
    absl::Span<ast::Call::Argument const> const &arguments,
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

  ir::CompleteResultBuffer buffer;
  auto constant_arguments = EmitConstantArguments(c, arguments, buffer);

  EmitArguments(c, param_qts, {/* TODO: Defaults */}, arguments,
                constant_arguments, prepared_arguments);
  return std::make_pair(init, std::move(prepared_arguments));
}

void InlineStartIntoCurrent(Compiler &c, ir::BasicBlock *entry_block,
                            std::optional<ir::Reg> state_ptr,
                            ir::CompiledScope const &compiled_scope,
                            ir::BasicBlock *starting_block,
                            absl::Span<ast::Call::Argument const> arguments) {
  c.builder().UncondJump(starting_block);
  c.builder().CurrentBlock() = starting_block;

  auto [init, args] =
      EmitIrForJumpArguments(c, state_ptr, arguments, compiled_scope);
  c.builder().InlineJumpIntoCurrent(init, args,
                                    c.state().scope_landings.back());
}

struct ScopeBlockDescription {
  // The begining of the `start` block. We need to be able to jump here in case
  // the block arguments need to be reevaluated.
  ir::BasicBlock *starting_block;

  // If a scope has a path that exits normally along some path (a call to exit
  // rather than a return or labelled yield), then this is the block on which
  // we land.
  ir::BasicBlock *landing_block;

  // Block metadata keyed on the names of the block. The string names are all
  // effectively ephemeral: They are either string literals or they are owned by
  // the AST which outlives this struct.
  absl::flat_hash_map<std::string_view, BlockMetadata> blocks_by_name;

  // A map that holds a subset of the information in `blocks_by_name` that is
  // required by ScopeState.
  absl::flat_hash_map<std::string_view, ir::BasicBlock *> names;
};

// Returns a description of all blocks present in this scope, relating their
// names, AST nodes, and generated `ir::BasicBlocks`. This function does not
// modify `builder().CurrentBlock()`.
ScopeBlockDescription MakeScopeBlockDescription(
    ir::Builder& builder,
    absl::Span<ast::BlockNode const> blocks) {
  ScopeBlockDescription result;
  result.starting_block = builder.AddBlock("scope-start");
  result.landing_block  = builder.AddBlock("scope-done");

  result.blocks_by_name.emplace(
      "start",
      BlockMetadata{.block_node = nullptr, .block = result.starting_block});
  result.blocks_by_name.emplace(
      "done",
      BlockMetadata{.block_node = nullptr, .block = result.landing_block});

  result.names.emplace("start", result.starting_block);
  result.names.emplace("done", result.landing_block);

  for (auto const &block_node : blocks) {
    auto *b = builder.AddBlock(
        absl::StrFormat("Body of block `%s`.", block_node.name()));
    result.blocks_by_name.emplace(
        block_node.name(),
        BlockMetadata{.block_node = &block_node, .block = b});
    result.names.emplace(block_node.name(), b);

    // TODO: Add Phi nodes onto each of these blocks.
  }
  return result;
}

template <typename T>
void MakePhi(ir::Builder &builder, std::string_view name,
             ir::RegOr<ir::addr_t> addr, ir::ScopeState &state) {
  ir::PhiInstruction<T> *phi = builder.PhiInst<T>();
  state.set_phis[name].push_back(
      [phi](ir::BasicBlock const *block, ir::Reg r) { phi->add(block, r); });
  builder.Store<ir::RegOr<T>>(phi->result, addr);
}

}  // namespace

void Compiler::EmitToBuffer(ast::ScopeNode const *node,
                            ir::PartialResultBuffer &out) {
  LOG("ScopeNode", "Emitting IR for ScopeNode");
  ir::Scope scope = *EvaluateOrDiagnoseAs<ir::Scope>(node->name());

  // Emit IR for each block node, emission for each block node handles jumping
  // to the correct location, including jumps as well as subsequent calls to
  // before. This means that we have to ensure the body blocks already exist.
  auto [starting_block, landing_block, blocks_by_name, names] =
      MakeScopeBlockDescription(builder(), node->blocks());

  // Push the scope landing state onto the the vector. Any nested scopes will be
  // able to lookup the label in this vector to determine where they should jump
  // to.
  auto const *compiled_scope = ir::CompiledScope::From(scope);

  // If the scope is stateful, stack-allocate space for the state and
  // default-initialize it.
  //
  // TODO: This interface isn't great beacuse it requires default-initializable
  // state and no way to call any other initializer.
  auto *entry_block = builder().CurrentBlock();
  std::optional<ir::Reg> state_ptr = InitializeScopeState(*this, *compiled_scope);

  state().scope_landings.push_back(ir::ScopeState{
      .label = node->label() ? node->label()->value() : ir::Label(),
      .scope = scope,
      // TODO: Implement me
      .result_type = type::QualType::NonConstant(type::Void),
      .block       = landing_block,
      .names       = std::move(names),
      .state       = state_ptr,
  });
  absl::Cleanup c = [&] { state().scope_landings.pop_back(); };

  // Allocations for block parameters need to happen before we emit IR for any
  // block itself.
  for (auto [name, metadata] : blocks_by_name) {
    builder().CurrentBlock() = metadata.block;
    // TODO: start/done blocks need to be processed too.
    if (not metadata.block_node) { continue; }
    auto const &params = metadata.block_node->params();
    for (auto const &decl : params) {
      auto const *param = decl.value.get();
      auto addr = builder().Alloca(context().qual_types(param)[0].type());
      // TODO: Support multiple declarations?
      builder().set_addr(&param->ids()[0], addr);
      type::Type t = context().qual_types(param)[0].type();
      if (t.is_big()) {
        ir::PhiInstruction<ir::addr_t> *phi = builder().PhiInst<ir::addr_t>();
        state().scope_landings.back().set_phis[name].push_back(
            [phi](ir::BasicBlock const *block, ir::Reg r) {
              phi->add(block, r);
            });

        ir::PartialResultBuffer from_buffer;
        from_buffer.append(phi->result);
        EmitMoveInit(type::Typed(addr, t), from_buffer);
      } else if (auto const *p = t.if_as<type::Primitive>()) {
        p->Apply([ this, addr, name = std::string_view(name) ]<typename T>() {
          MakePhi<T>(builder(), name, addr, state().scope_landings.back());
        });
      } else if (t.is<type::Enum>()) {
        MakePhi<type::Enum::underlying_type>(builder(), name, addr,
                                             state().scope_landings.back());
      } else if (t.is<type::Flags>()) {
        MakePhi<type::Flags::underlying_type>(builder(), name, addr,
                                              state().scope_landings.back());
      } else {
        NOT_YET(t);
      }
    }
  }

  builder().CurrentBlock() = entry_block;
  InlineStartIntoCurrent(*this, entry_block, state_ptr, *compiled_scope,
                         starting_block, node->arguments());

  for (auto [name, metadata] : blocks_by_name) {
    if (not metadata.block_node) { continue; }
    builder().CurrentBlock() = metadata.block;
    EmitVoid(metadata.block_node);
  }

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
