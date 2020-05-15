#ifndef ICARUS_IR_BLOCKS_BASIC_H
#define ICARUS_IR_BLOCKS_BASIC_H

#include <iostream>
#include <memory>
#include <vector>

#include "base/meta.h"
#include "base/ptr_span.h"
#include "base/untyped_buffer.h"
#include "ir/blocks/load_store_cache.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/base.h"
#include "ir/instruction/jump.h"
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

  LoadStoreCache &load_store_cache() { return cache_; }

  void erase_incoming(BasicBlock *b) { incoming_.erase(b); }

  base::PtrSpan<Instruction const> instructions() const {
    return instructions_;
  }

  void AddInstruction(std::unique_ptr<Instruction> inst) {
    instructions_.push_back(std::move(inst));
  }

  void set_jump(JumpCmd j) { jump_ = std::move(j); }
  JumpCmd const &jump() const { return jump_; }

  void WriteByteCode(ByteCodeWriter *writer);

  friend std::ostream &operator<<(std::ostream &os, BasicBlock const &b);

 private:
  friend struct ir::InstructionInliner;

  void RemoveOutgoingJumps();
  void AddOutgoingJumps(JumpCmd const &jump);
  void ExchangeJumps(BasicBlock const *b);

  std::vector<std::unique_ptr<Instruction>> instructions_;

 LoadStoreCache cache_;
  absl::flat_hash_set<BasicBlock *> incoming_;

  JumpCmd jump_ = JumpCmd::Return();
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCKS_BASIC_H