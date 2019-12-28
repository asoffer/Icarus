#ifndef ICARUS_IR_BASIC_BLOCK_H
#define ICARUS_IR_BASIC_BLOCK_H

#include <iostream>
#include <memory>
#include <vector>

#include "base/untyped_buffer.h"
#include "core/alignment.h"
#include "core/bytes.h"
#include "ir/cmd/jump.h"
#include "ir/instructions.h"
#include "ir/out_params.h"

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

  void Serialize() {
    for (auto const &inst : instructions_) { inst->Serialize(&cmd_buffer_); }
  }

 private:
  void RemoveOutgoingJumps();
  void AddOutgoingJumps(JumpCmd const &jump);
  void ExchangeJumps(BasicBlock const *b);

  friend struct Builder;
  friend struct Inliner;
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
