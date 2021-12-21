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

template <typename T>
void MakePhi(ir::Builder &builder, std::string_view name,
             ir::RegOr<ir::addr_t> addr, ir::ScopeState &state) {
  ir::PhiInstruction<T> *phi = builder.PhiInst<T>();
  state.set_phis[name].push_back(
      [phi](ir::BasicBlock const *block, ir::Reg r) { phi->add(block, r); });
  builder.Store<ir::RegOr<T>>(phi->result, addr);
}

// BasicBlockMapping tracks a mapping from basic blocks in a scope being inlined
// to their corresponding created basic block in their inlined location.
struct BasicBlockMapping {
  explicit BasicBlockMapping(ir::Builder &builder, ir::Scope scope) {
    for (auto const *block : scope->blocks()) {
      auto [iter, inserted] =
          mapping_.try_emplace(block, builder.AddBlock(*block));
      ASSERT(inserted == true);
    }
  }

  ir::BasicBlock *operator()(ir::BasicBlock const *from) const {
    auto iter = mapping_.find(from);
    ASSERT(iter != mapping_.end());
    return iter->second;
  }

  auto begin() const { return mapping_.begin(); }
  auto end() const { return mapping_.end(); }

 private:
  absl::flat_hash_map<ir::BasicBlock const *, ir::BasicBlock *> mapping_;
};

ir::BasicBlock *InlineScope(
    Compiler &c, ir::Scope to_be_inlined,
    ir::PartialResultBuffer const &arguments,
    absl::Span<std::pair<ir::BasicBlock *, ir::BasicBlock *> const>
        block_entry_exit) {
  auto landing = c.builder().AddBlock();

  auto *start_block          = c.builder().CurrentBlock();
  size_t inlined_start_index = c.builder().CurrentGroup()->blocks().size();

  auto *into                 = c.builder().CurrentGroup();
  c.builder().CurrentBlock() = start_block;

  // Update the register count. This must be done after we've added the
  // register-forwarding instructions which use this count to choose a register
  // number.
  ir::Inliner inliner(into->num_regs(), to_be_inlined->num_args());

  // TODO: Parameter binding.

  into->MergeAllocationsFrom(*to_be_inlined, inliner);
  BasicBlockMapping mapping(c.builder(), to_be_inlined);
  for (auto [from_block, to_block] : mapping) {
    base::Traverse(inliner, *to_block);

    to_block->jump().Visit([&, jump = &to_block->jump()](auto &j) {
      constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
      if constexpr (type == base::meta<ir::JumpCmd::UncondJump>) {
        j.block = mapping(j.block);
      } else if constexpr (type == base::meta<ir::JumpCmd::CondJump>) {
        base::Traverse(inliner, j.reg);
        j.true_block  = mapping(j.true_block);
        j.false_block = mapping(j.false_block);
      } else if constexpr (type == base::meta<ir::JumpCmd::RetJump>) {
        *jump = ir::JumpCmd::Uncond(landing);
      } else if constexpr (type == base::meta<ir::JumpCmd::UnreachableJump>) {
        return;
      } else if constexpr (type == base::meta<ir::JumpCmd::BlockJump>) {
        auto [entry, exit] = block_entry_exit[j.block.value()];
        auto exit_block    = mapping(to_be_inlined.connection(j.block).second);
        *jump              = ir::JumpCmd::Uncond(entry);
        c.builder().CurrentBlock() = exit;
        c.builder().UncondJump(exit_block);
      } else {
        UNREACHABLE(type);
      }
    });
  }

  LOG("", "%p %p", start_block, mapping(to_be_inlined->entry()));
  start_block->set_jump(ir::JumpCmd::Uncond(mapping(to_be_inlined->entry())));
  return landing;
}

}  // namespace

void Compiler::EmitToBuffer(ast::ScopeNode const *node,
                            ir::PartialResultBuffer &out) {
  LOG("ScopeNode", "Emitting IR for ScopeNode");
  auto unbound_scope = *EvaluateOrDiagnoseAs<ir::UnboundScope>(node->name());

  ir::ScopeContext scope_context = context().ScopeContext(node);
  ir::Scope scope = unbound_scope.bind(work_resources(), scope_context);

  ast::ScopeLiteral const *scope_lit = context().AstLiteral(unbound_scope);

  auto find_subcontext_result =
      FindInstantiation(*this, scope_lit, scope_context);
  auto &context = find_subcontext_result.context;

  EnsureComplete({
      .kind    = WorkItem::Kind::EmitScopeBody,
      .node    = scope_lit,
      .context = &context,
  });

  auto *start              = builder().CurrentBlock();

  std::vector<std::pair<ir::BasicBlock *, ir::BasicBlock *>> block_entry_exit;
  block_entry_exit.reserve(node->blocks().size());
  for (auto const &block : node->blocks()) {
    auto &[entry, exit]      = block_entry_exit.emplace_back();
    entry                    = builder().AddBlock();
    builder().CurrentBlock() = entry;
    ir::PartialResultBuffer ignored;
    EmitToBuffer(&block, ignored);
    exit = builder().CurrentBlock();
  }

  builder().CurrentBlock() = start;
  ir::PartialResultBuffer argument_buffer;
  EmitArguments(*this, scope.type()->params(), {/* TODO: Defaults */},
                node->arguments(), {/* TODO: Constant arguments */},
                argument_buffer);

  builder().CurrentBlock() =
      InlineScope(*this, scope, argument_buffer, block_entry_exit);
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
