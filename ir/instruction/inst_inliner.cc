#include "ir/instruction/inst_inliner.h"

#include <atomic>
#include <queue>
#include <string_view>
#include <type_traits>
#include <utility>

#include "ir/instruction/inliner.h"

#include "absl/container/flat_hash_set.h"
#include "base/meta.h"

namespace ir {

void InstructionInliner::InlineJumpOnBlock(BasicBlock* block) {
  block->jump().Visit([&](auto& j) {
    constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
    if constexpr (type == base::meta<JumpCmd::UnreachableJump>) {
      UNREACHABLE(*block);
    } else if constexpr (type == base::meta<JumpCmd::RetJump>) {
      UNREACHABLE(*block);
    } else if constexpr (type == base::meta<JumpCmd::JumpExitJump>) {
      SetAndInlineArguments(j, block);
      block->set_jump(JumpCmd::Uncond(block_for(j.name)));
    } else if constexpr (type == base::meta<JumpCmd::UncondJump>) {
      Inline(j.block, block);
    } else if constexpr (type == base::meta<JumpCmd::CondJump>) {
      inliner_(j.reg);
      Inline(j.true_block, block);
      Inline(j.false_block, block);
    } else if constexpr (type == base::meta<JumpCmd::ChooseJump>) {
      auto [index, unused] = block_for(j.names());
      std::string_view next_name = j.names()[index];
      block->set_jump(JumpCmd::Uncond(j.blocks()[index]));
      InlineJumpOnBlock(block);
    } else {
      static_assert(base::always_false<type>());
    }
  });
}

BasicBlock* InstructionInliner::block_for(std::string_view name) const {
  auto iter = blocks_by_name_.find(name);
  if (iter == blocks_by_name_.end()) { return nullptr; }
  return iter->second;
}

std::pair<size_t, BasicBlock*> InstructionInliner::block_for(
    absl::Span<std::string_view const> names) const {
  size_t i = 0;
  for (std::string_view name : names) {
    if (auto* b = block_for(name)) { return std::pair(i, b); }
    ++i;
  }
  return std::pair(std::numeric_limits<size_t>::max(), nullptr);
}

InstructionInliner::InstructionInliner(
    internal::BlockGroupBase const* to_be_inlined,
    internal::BlockGroupBase* into,
    absl::flat_hash_map<std::string_view, BasicBlock*> blocks_by_name)
    : to_be_inlined_(to_be_inlined),
      into_(into),
      register_offset_(into->num_regs()),
      blocks_by_name_(std::move(blocks_by_name)),
      inliner_(register_offset_, to_be_inlined_->num_args()) {
  FindBlocksToInline();
}

void InstructionInliner::FindBlocksToInline() {
  // Cluster generation is a debugging facility. It assigns the same value to
  // each basic block in a to-be-inlined group of blocks so that when generating
  // control-flow graphs, we see where blocks were generated from.
  static std::atomic<uint64_t> cluster_index_generator(1);
  uint64_t index =
      cluster_index_generator.fetch_add(1, std::memory_order_relaxed);

  std::queue<BasicBlock const*> to_visit;
  to_visit.push(to_be_inlined_->entry());
  while (not to_visit.empty()) {
    auto const* block = to_visit.front();
    to_visit.pop();
    auto [iter, inserted] = to_rewrite_.try_emplace(block);
    if (not inserted) { continue; }

    iter->second = into_->AppendBlock(
        *block, BasicBlock::DebugInfo{
                    .header = absl::StrCat("Jump: ", block->debug().header),
                    .cluster_index = index});

    block->jump_.Visit([&](auto& j) {
      constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
      if constexpr (type == base::meta<JumpCmd::RetJump> or
                    type == base::meta<JumpCmd::JumpExitJump>) {
        // Nothing to do.
      } else if constexpr (type == base::meta<JumpCmd::UncondJump>) {
        to_visit.push(j.block);
      } else if constexpr (type == base::meta<JumpCmd::CondJump>) {
        to_visit.push(j.true_block);
        to_visit.push(j.false_block);
      } else if constexpr (type == base::meta<JumpCmd::ChooseJump>) {
        auto [i, unused] = block_for(j.names());
        ASSERT(i < j.args().size());
        choose_argument_cache_.emplace(block, &j.args()[i]);
        to_visit.push(j.blocks()[i]);
      } else {
        UNREACHABLE(*block);
      }
    });
  }
}

void InstructionInliner::Inline(BasicBlock*& block,
                                BasicBlock* incoming_block) const {
  auto iter = to_rewrite_.find(block);
  ASSERT(iter != to_rewrite_.end());
  block = iter->second;
  block->incoming_.insert(incoming_block);
}

void InstructionInliner::InlineAllBlocks() {
  // Update the register count. This must be done after we've added the
  // register-forwarding instructions which use this count to choose a register
  // number.
  into_->alloc_.MergeFrom(to_be_inlined_->alloc_, inliner_);

  for (auto [ignored, block] : to_rewrite_) {
    base::Traverse(inliner_, *block);
    InlineJumpOnBlock(block);
  }

  block_for("start")->set_jump(
      JumpCmd::Uncond(to_rewrite_.at(to_be_inlined_->entry())));
}

}  // namespace ir
