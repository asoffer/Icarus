#ifndef ICARUS_IR_BLOCKS_BASIC_H
#define ICARUS_IR_BLOCKS_BASIC_H

#include <iostream>
#include <memory>
#include <vector>

#include "base/ptr_span.h"
#include "base/untyped_buffer.h"
#include "core/alignment.h"
#include "core/bytes.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/base.h"
#include "ir/instruction/jump.h"
#include "ir/results.h"

namespace ir {
namespace internal {
struct BlockGroup;
}  // namespace internal

// BasicBlock:
//
// A basic block is an ordered collection of instructions with no branches. When
// executed each instruction will execute in order. `BasicBlock`s also have a
// jump describing which basic block to execute next.
struct BasicBlock {
  // Each BasicBlock belongs to a `BlockGroup` and needs a pointer back to that group.
  explicit BasicBlock(internal::BlockGroup *group) : group_(group) {}

  BasicBlock(BasicBlock const &b) noexcept;
  BasicBlock(BasicBlock &&) noexcept;
  BasicBlock &operator=(BasicBlock const &) noexcept;
  BasicBlock &operator=(BasicBlock &&) noexcept;

  // Copies the block and updates the internal block group to `group`.
  explicit BasicBlock(BasicBlock const &b, internal::BlockGroup *group) noexcept
      : BasicBlock(b) {
    group_ = group;
  }

  void ReplaceJumpTargets(BasicBlock *old_target, BasicBlock *new_target);
  void Append(BasicBlock &&b);

  // Returns the `BlockGroup` this `BasicBlock` belongs to.
  internal::BlockGroup *group() { return group_; }
  internal::BlockGroup const *group() const { return group_; }

  // While building basic blocks, we cache information that is known about the
  // values/registers stored in each allocation on this block. On each load, we
  // cache the register the value is loaded into so that subsequent loads from
  // the same address can reuse this register rather than emitting a redundant
  // load instruction. The cache is invalidated conservatively. Any function
  // call, or any store will invalidate the cache.
  //
  // TODO stores should only erase the one value that was cached, though if the
  // location we're storing to is not a constant, we may need to invalidate
  // more. Perhaps invalidating every cache entry for the given type?
  Results &CacheSlot(Reg r) { return storage_cache_[r]; }
  void ClearCache() { storage_cache_.clear(); }

  // All `BasicBlocks` which can jump to this one. Some may jump unconditionally
  // whereas others may jump only conditionally.
  auto const &incoming() const { return incoming_; }

  void insert_incoming(BasicBlock *b) {
    incoming_.insert(b);
    b->jump_ = JumpCmd::Uncond(this);
  }

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
  internal::BlockGroup *group_;

  // A cache of what values we know are held in these addresses.
  absl::flat_hash_map<Reg, Results> storage_cache_;
  absl::flat_hash_set<BasicBlock *> incoming_;

  JumpCmd jump_ = JumpCmd::Return();
};

}  // namespace ir

#endif  // ICARUS_IR_BLOCKS_BASIC_H
