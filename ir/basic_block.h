#ifndef ICARUS_IR_BASIC_BLOCK_H
#define ICARUS_IR_BASIC_BLOCK_H

#include <iostream>
#include <memory>
#include <vector>

#include "base/untyped_buffer.h"
#include "core/alignment.h"
#include "core/bytes.h"
#include "ir/cmd/jump.h"
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

  base::untyped_buffer cmd_buffer_;
  size_t num_incoming() const { return incoming_.size(); }

 private:
  void RemoveOutgoingJumps();
  void AddOutgoingJumps(JumpCmd const &jump);
  void ExchangeJumps(BasicBlock const *b);

  friend struct Builder;
  friend struct Inliner;
  friend std::ostream &operator<<(std::ostream &os, BasicBlock const &b);

  internal::BlockGroup *group_;

 public:
  JumpCmd jump_ = JumpCmd::Return();
  absl::flat_hash_set<BasicBlock *> incoming_;
};

Reg Reserve(core::Bytes b, core::Alignment a);

template <typename T>
Reg MakeResult() {
  return Reserve(core::Bytes::Get<T>(), core::Alignment::Get<T>());
}

Reg MakeResult(type::Type const *t);

}  // namespace ir
#endif  // ICARUS_IR_BASIC_BLOCK_H
