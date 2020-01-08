#ifndef ICARUS_IR_INSTRUCTION_INLINER_H
#define ICARUS_IR_INSTRUCTION_INLINER_H

#include <string_view>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "ir/basic_block.h"
#include "ir/block_group.h"
#include "ir/cmd/jump.h"
#include "ir/local_block_interpretation.h"
#include "ir/reg.h"
#include "ir/reg_or.h"
#include "ir/results.h"

namespace ir {
struct BasicBlock;

struct InstructionInliner {
  explicit InstructionInliner(internal::BlockGroup *to_be_inlined,
                              internal::BlockGroup *into,
                              LocalBlockInterpretation block_interp);

  void Inline(Reg &r) const;
  void Inline(Results &r) const;
  void Inline(BasicBlock *&block, BasicBlock *incoming_block) const;
  void InlineJump(BasicBlock *block);

  template <typename T>
  void Inline(RegOr<T> &r) const {
    if (r.is_reg()) {
      Reg copy = r.reg();
      Inline(copy);
      r = copy;
    }
  }

  void InlineAllBlocks();

  absl::flat_hash_map<
      std::string_view,
      std::pair<BasicBlock *, core::FnArgs<type::Typed<ir::Results>>>>
  ExtractNamedBlockMapping() {
    return std::move(named_blocks_);
  }

  ir::BasicBlock *landing_block() const { return landing_block_; }

  template <typename T>
  void Inline(std::vector<RegOr<T>> &rs) const {
    for (auto &r : rs) { Inline(r); }
  }

 private:
  BasicBlock *CorrespondingBlock(BasicBlock *block) {
    return blocks_.find(block)->second;
  }

  internal::BlockGroup *to_be_inlined_;
  internal::BlockGroup *into_;
  int register_offset_;
  absl::flat_hash_map<BasicBlock const *, BasicBlock *> blocks_;

  absl::flat_hash_map<
      std::string_view,
      std::pair<BasicBlock *, core::FnArgs<type::Typed<ir::Results>>>>
      named_blocks_;
  LocalBlockInterpretation block_interp_;

  // When inlining a return jump, this is the block that should be
  // unconditionally jumped to.
  ir::BasicBlock *landing_block_;
};
}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_INLINER_H
