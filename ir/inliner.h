#ifndef ICARUS_IR_CMD_INLINER_H
#define ICARUS_IR_CMD_INLINER_H

#include "ir/register.h"
#include "ir/reg.h"

namespace type {
struct Type;
}  // namespace type

namespace ir {
struct CompiledFn;
struct StackFrameAllocations;

struct Inliner {

  void Inline(Reg *r, type::Type const *t = nullptr) const;

  constexpr void Inline(BlockIndex *b) const {
    *b = BlockIndex(b->value + block_offset_);
  }

  void MergeAllocations(CompiledFn *fn, StackFrameAllocations const &allocs);

  BlockIndex landing() const { return land_; }

 private:
  friend struct CompiledFn;
  explicit Inliner(size_t reg_offset, size_t block_offset, BlockIndex land)
      : reg_offset_(reg_offset),
        block_offset_(block_offset),
        land_(land) {}

  size_t reg_offset_   = 0;
  size_t block_offset_ = 0;
  BlockIndex land_{};
};

}  // namespace ir

#endif  // ICARUS_IR_CMD_INLINER_H
