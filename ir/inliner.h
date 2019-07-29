#ifndef ICARUS_IR_CMD_INLINER_H
#define ICARUS_IR_CMD_INLINER_H

#include "ir/register.h"
#include "ir/reg.h"

namespace ir {

struct Inliner {
  constexpr void Inline(Reg *r) const { *r = Reg{r->value() + reg_offset_}; }
  constexpr void Inline(BlockIndex *b) const {
    *b = BlockIndex(b->value + block_offset_);
  }

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
