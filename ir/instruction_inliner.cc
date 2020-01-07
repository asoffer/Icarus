#include "ir/instruction_inliner.h"

namespace ir {

InstructionInliner::InstructionInliner(internal::BlockGroup* to_be_inlined,
                                       internal::BlockGroup* into,
                                       LocalBlockInterpretation block_interp)
    : to_be_inlined_(to_be_inlined),
      into_(into),
      register_offset_(into->num_regs()),
      block_interp_(std::move(block_interp)) {
  DEBUG_LOG("InstructionInliner")("Inlining: ", *to_be_inlined);
  DEBUG_LOG("InstructionInliner")("Into: ", *into);

  for (auto* block_to_copy : to_be_inlined->blocks()) {
    // Copy the block and then scan it for references to things that need to
    // be changed with inlining (e.g., basic blocks or registers).
    //
    // Note that we need to copy all blocks first (or at least allocate them)
    // because we may request a jump downwards (i.e., to a block which we have
    // not yet seen). In other words, we have to make sure that any jump which
    // needs to be updated, the block mapping is already present.
    auto* block = into->AppendBlock(*block_to_copy);
    blocks_.emplace(block_to_copy, block);
  }

  landing_block_ = into->AppendBlock();
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
    using type = std::decay_t<decltype(j)>;
    if constexpr (std::is_same_v<type, JumpCmd::RetJump>) {
      landing_block_->incoming_.insert(block);
      block->jump_ = JumpCmd::Uncond(landing_block_);

    } else if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
      Inline(j.block, block);
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
      Inline(j.reg);
      Inline(j.true_block, block);
      Inline(j.false_block, block);
    } else if constexpr (std::is_same_v<type, JumpCmd::ChooseJump>) {
      std::string_view next_name = "";
      for (std::string_view name : j.blocks()) {
        if (name == "start" or name == "exit" or
            block_interp_.block_node(name)) {
          next_name = name;
          break;
        }
      }
      ASSERT(next_name != "");

      auto& entry = named_blocks_[next_name];
      if (entry.first == nullptr) {
        entry.first = block->group()->AppendBlock();
      }
      block->jump_ = JumpCmd::Uncond(entry.first);
      entry.first->incoming_.insert(block);
    } else {
      static_assert(base::always_false<type>());
    }
  });
}

void InstructionInliner::InlineAllBlocks() {
  // Update the register count. This must be done after we've added the
  // register-forwarding instructions which use this count to choose a register
  // number.
  into_->num_regs_ += to_be_inlined_->num_regs();

  for (auto [ignored, block] : blocks_) {
    DEBUG_LOG("inliner-before")(*block);

    for (auto& inst : block->instructions_) { inst->Inline(*this); }
    InlineJump(block);

    DEBUG_LOG("inliner-after")(*block);
  }
}

}  // namespace ir
