#ifndef ICARUS_IR_BLOCKS_BASIC_H
#define ICARUS_IR_BLOCKS_BASIC_H

#include <iostream>
#include <memory>
#include <vector>

#include "base/meta.h"
#include "base/untyped_buffer.h"
#include "ir/blocks/load_store_cache.h"
#include "ir/blocks/offset_cache.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/base.h"
#include "ir/instruction/jump.h"
#include "ir/instruction/op_codes.h"
#include "ir/value/addr.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"

namespace ir {

// BasicBlock:
//
// A basic block is an ordered collection of instructions with no branches. When
// executed, each instruction will execute in order. `BasicBlock`s also have a
// jump describing which basic block to execute next.
struct BasicBlock {
  BasicBlock() = default;
  BasicBlock(BasicBlock const &b) noexcept;
  BasicBlock(BasicBlock &&) noexcept;
  BasicBlock &operator=(BasicBlock const &) noexcept;
  BasicBlock &operator=(BasicBlock &&) noexcept;

  void ReplaceJumpTargets(BasicBlock *old_target, BasicBlock *new_target);
  void Append(BasicBlock &&b);

  // All `BasicBlocks` which can jump to this one. Some may jump unconditionally
  // whereas others may jump only conditionally.
  auto const &incoming() const { return incoming_; }

  void insert_incoming(BasicBlock *b) {
    incoming_.insert(b);
    b->jump_ = JumpCmd::Uncond(this);
  }

  LoadStoreCache &load_store_cache() { return load_store_cache_; }
  OffsetCache &offset_cache() { return offset_cache_; }

  void erase_incoming(BasicBlock *b) { incoming_.erase(b); }

  absl::Span<Inst const> instructions() const { return instructions_; }

  // TODO: We really don't need to be returning this reference.
  Inst &Append(Inst &&inst) {
    return instructions_.emplace_back(std::move(inst));
  }

  void set_jump(JumpCmd j) { jump_ = std::move(j); }
  JumpCmd const &jump() const { return jump_; }

  template <typename InstructionSet>
  void WriteByteCode(ByteCodeWriter *writer) {
    writer->StartBlock(this);
    for (auto const &inst : instructions_) {
      if (not inst) { continue; }
      writer->Write(InstructionSet::Index(inst));
      inst.WriteByteCode(writer);
    }
    jump_.Visit([&](auto &j) {
      using type = std::decay_t<decltype(j)>;
      if constexpr (std::is_same_v<type, JumpCmd::RetJump>) {
        writer->Write(internal::kReturnInstruction);
      } else if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
        writer->Write(internal::kUncondJumpInstruction);
        writer->Write(j.block);
      } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
        writer->Write(internal::kCondJumpInstruction);
        writer->Write(j.reg);
        writer->Write(j.true_block);
        writer->Write(j.false_block);
      }
    });
  }

  friend std::ostream &operator<<(std::ostream &os, BasicBlock const &b);

 private:
  friend struct ir::InstructionInliner;

  void RemoveOutgoingJumps();
  void AddOutgoingJumps(JumpCmd const &jump);
  void ExchangeJumps(BasicBlock const *b);

  std::vector<Inst> instructions_;

  LoadStoreCache load_store_cache_;
  OffsetCache offset_cache_;
  absl::flat_hash_set<BasicBlock *> incoming_;

  JumpCmd jump_ = JumpCmd::Return();
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCKS_BASIC_H
