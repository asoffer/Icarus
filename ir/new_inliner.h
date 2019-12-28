#ifndef ICARUS_IR_NEW_INLINER_H
#define ICARUS_IR_NEW_INLINER_H

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "ir/cmd/jump.h"
#include "ir/reg.h"
#include "ir/reg_or.h"

namespace ir {
struct BasicBlock;

struct Inliner {
  explicit Inliner(int register_offset,
                   absl::flat_hash_map<BasicBlock const *, BasicBlock *> blocks,
                   ir::BasicBlock *landing_block)
      : register_offset_(register_offset),
        blocks_(std::move(blocks)),
        landing_block_(landing_block) {}

  void Inline(Reg &r) const;
  void Inline(BasicBlock *&block) const;
  void Inline(JumpCmd &j, BasicBlock *current_block) const;

  template <typename T>
  void Inline(RegOr<T> &r) const {
    if (r.is_reg()) {
      Reg copy = r.reg();
      Inline(copy);
      r = copy;
    }
  }


  ir::BasicBlock *landing_block() const { return landing_block_; }

  template <typename T>
  void Inline(std::vector<RegOr<T>> &rs) const {
    for (auto &r : rs) { Inline(r); }
  }

 private:
  int register_offset_;
  absl::flat_hash_map<BasicBlock const *, BasicBlock *> blocks_;

  // When inlining a return jump, this is the block that should be
  // unconditionally jumped to.
  ir::BasicBlock * landing_block_;
};
}  // namespace ir

#endif  // ICARUS_IR_NEW_INLINER_H
