#ifndef ICARUS_IR_BASIC_BLOCK_H
#define ICARUS_IR_BASIC_BLOCK_H

#include <iostream>
#include <memory>
#include <vector>

#include "base/untyped_buffer.h"
#include "core/alignment.h"
#include "core/bytes.h"
#include "ir/byte_code_writer.h"
#include "ir/cmd/jump.h"
#include "ir/instructions_base.h"
#include "ir/out_params.h"
#include "ir/results.h"

namespace ir {
namespace internal {
struct BlockGroup;
}  // namespace internal

struct BasicBlock {
  BasicBlock() = default;
  explicit BasicBlock(internal::BlockGroup *group) : group_(group) {}

  BasicBlock(BasicBlock const &b);
  BasicBlock(BasicBlock &&) noexcept;
  BasicBlock &operator=(BasicBlock const &);
  BasicBlock &operator=(BasicBlock &&) noexcept;

  void ReplaceJumpTargets(BasicBlock *old_target, BasicBlock *new_target);
  void Append(BasicBlock const &b);

  std::vector<std::unique_ptr<Instruction>> instructions_;
  base::untyped_buffer cmd_buffer_;
  size_t num_incoming() const { return incoming_.size(); }

  void WriteByteCode() {
    // TODO rather than clearing something that's been previously serialized, we
    // should only serialize once. We shouldn't store the serialized version
    // here at all.
    cmd_buffer_.clear();
    ByteCodeWriter writer(&cmd_buffer_);
    writer.StartBlock(this);
    for (auto const &inst : instructions_) { inst->WriteByteCode(&writer); }
    writer.replacements_.clear(); // TODO
    writer.MakeReplacements();
  }

  internal::BlockGroup *group() { return group_; }
  internal::BlockGroup const *group() const { return group_; }

 private:
  void RemoveOutgoingJumps();
  void AddOutgoingJumps(JumpCmd const &jump);
  void ExchangeJumps(BasicBlock const *b);

  friend struct Builder;
  friend struct Inliner;
  friend struct internal::BlockGroup;
  friend std::ostream &operator<<(std::ostream &os, BasicBlock const &b);

  internal::BlockGroup *group_;


 public:
  // A cache of what values we know are held in these addresses.
  absl::flat_hash_map<Reg, Results> storage_cache_;

  JumpCmd jump_ = JumpCmd::Return();
  absl::flat_hash_set<BasicBlock *> incoming_;

};

}  // namespace ir
#endif  // ICARUS_IR_BASIC_BLOCK_H
