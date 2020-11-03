#include <atomic>
#include <queue>
#include <string_view>
#include <type_traits>
#include <utility>

#include "ir/instruction/inliner.h"

#include "absl/container/flat_hash_set.h"
#include "base/meta.h"
#include "type/typed_value.h"

namespace ir {

InstructionInliner::InstructionInliner(
    internal::BlockGroupBase const* to_be_inlined,
    internal::BlockGroupBase* into, LocalBlockInterpretation block_interp)
    : to_be_inlined_(to_be_inlined),
      into_(into),
      register_offset_(into->num_regs()),
      block_interp_(std::move(block_interp)) {
  LOG("InstructionInliner", "Inlining: %s", *to_be_inlined);
  LOG("InstructionInliner", "Into: %s", *into);

  static std::atomic<uint64_t> cluster_index_generator(1);
  uint64_t index =
      cluster_index_generator.fetch_add(1, std::memory_order_relaxed);

  absl::flat_hash_set<BasicBlock const*> to_inline;
  std::queue<BasicBlock const*> to_visit;
  to_visit.push(to_be_inlined->entry());
  while (not to_visit.empty()) {
    auto const* block = to_visit.front();
    to_visit.pop();
    auto [iter, inserted] = to_inline.insert(block);
    if (not inserted) { continue; }
    block->jump_.Visit([&](auto& j) {
      constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
      if constexpr (type == base::meta<JumpCmd::UncondJump>) {
        to_visit.push(j.block);
      } else if constexpr (type == base::meta<JumpCmd::CondJump>) {
        to_visit.push(j.true_block);
        to_visit.push(j.false_block);
      } else if constexpr (type == base::meta<JumpCmd::ChooseJump>) {
        std::string_view next_name = "";
        size_t i                   = 0;
        for (std::string_view name : j.names()) {
          if (name == "start" or name == "done" or
              block_interp_.block_node(name)) {
            next_name = name;
            break;
          }
          ++i;
        }
        ASSERT(next_name != "");

        // TODO: Rather than create a new block here, make sure choose jumps are
        // wired to their children so we can follow potentially many more such
        // blocks.
        auto& [next_block, args] = named_blocks_[next_name];
        if (next_block == nullptr) {
          next_block = into->AppendBlock(BasicBlock::DebugInfo{
              .header        = absl::StrCat("Landing to start ", next_name),
              .cluster_index = index});
        }
      } else {
        UNREACHABLE(*block);
      }
    });
  }

  for (auto* block_to_copy : to_inline) {
    // Copy the block and then scan it for references to things that need to
    // be changed with inlining (e.g., basic blocks or registers).
    //
    // Note that we need to copy all blocks first (or at least allocate them)
    // because we may request a jump downwards (i.e., to a block which we have
    // not yet seen). In other words, we have to make sure that any jump which
    // needs to be updated, the block mapping is already present.
    blocks_.emplace(
        block_to_copy,
        into->AppendBlock(
            *block_to_copy,
            BasicBlock::DebugInfo{
                .header = absl::StrCat("Jump: ", block_to_copy->debug().header),
                .cluster_index = index}));
  }
}

void InstructionInliner::Inline(Value& v) const {
  if (auto* r = v.get_if<Reg>()) { Inline(*r); }
}

void InstructionInliner::Inline(BasicBlock*& block,
                                BasicBlock* incoming_block) const {
  auto iter = blocks_.find(block);
  ASSERT(iter != blocks_.end());
  block = iter->second;
  block->incoming_.insert(incoming_block);
}

void InstructionInliner::Inline(Reg& r) const {
  if (r.is_arg()) {
    r = Reg(r.arg_value() + register_offset_);
  } else {
    r = Reg(r.value() + register_offset_);
  }
}

void InstructionInliner::InlineJump(BasicBlock* block) {
  block->jump_.Visit([&](auto& j) {
    constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
    if constexpr (type == base::meta<JumpCmd::UnreachableJump>) {
      // Nothing to do
    } else if constexpr (type == base::meta<JumpCmd::RetJump>) {
      UNREACHABLE(*block);
    } else if constexpr (type == base::meta<JumpCmd::UncondJump>) {
      Inline(j.block, block);
    } else if constexpr (type == base::meta<JumpCmd::CondJump>) {
      Inline(j.reg);
      Inline(j.true_block, block);
      Inline(j.false_block, block);
    } else if constexpr (type == base::meta<JumpCmd::ChooseJump>) {
      std::string_view next_name = "";
      size_t i                   = 0;
      for (std::string_view name : j.names()) {
        LOG("InlineJump", "%s", name);
        if (name == "start" or name == "done" or
            block_interp_.block_node(name)) {
          next_name = name;
          break;
        }
        ++i;
      }
      ASSERT(next_name != "");
      auto& [next_block, args] = named_blocks_[next_name];
      ASSERT(next_block != nullptr);
      args = j.args()[i].Transform([&](::type::Typed<Value> const& r) {
        auto copy = r;
        Inline(copy.get());
        return copy;
      });
      next_block->insert_incoming(block);
    } else {
      static_assert(base::always_false<type>());
    }
  });
}

BasicBlock* InstructionInliner::InlineAllBlocks() {
  // Update the register count. This must be done after we've added the
  // register-forwarding instructions which use this count to choose a register
  // number.
  into_->alloc_.MergeFrom(to_be_inlined_->alloc_, [&](Reg r) {
    Inline(r);
    return r;
  });

  for (auto [ignored, block] : blocks_) {
    for (auto& inst : block->instructions_) { inst->Inline(*this); }
    InlineJump(block);
  }

  return blocks_.at(to_be_inlined_->entry());
}

}  // namespace ir
