#ifndef ICARUS_IR_INSTRUCTION_INST_INLINER_H
#define ICARUS_IR_INSTRUCTION_INST_INLINER_H

#include <string_view>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "base/meta.h"
#include "ir/blocks/basic.h"
#include "ir/blocks/group.h"
#include "ir/instruction/jump.h"
#include "ir/local_block_interpretation.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace ir {
struct BasicBlock;

struct InstructionInliner {
  explicit InstructionInliner(internal::BlockGroupBase const *to_be_inlined,
                              internal::BlockGroupBase *into,
                              LocalBlockInterpretation block_interp);

  void Inline(BasicBlock *&block, BasicBlock *incoming_block) const;
  void InlineJump(BasicBlock *block);

  BasicBlock *InlineAllBlocks();

  absl::flat_hash_map<std::string,
                      std::vector<std::pair<Arguments, BasicBlock *>>>
  ArgumentsByName() && {
    return std::move(arguments_by_name_);
  }

 private:
  BasicBlock *CorrespondingBlock(BasicBlock *block) {
    return blocks_.find(block)->second;
  }

  internal::BlockGroupBase const *to_be_inlined_;
  internal::BlockGroupBase *into_;
  int register_offset_;
  absl::flat_hash_map<BasicBlock const *, BasicBlock *> blocks_;
  absl::flat_hash_map<std::string,
                      std::vector<std::pair<Arguments, BasicBlock *>>>
      arguments_by_name_;
  absl::flat_hash_map<BasicBlock const *, Arguments> choose_argument_cache_;
  LocalBlockInterpretation block_interp_;
  Inliner inliner_;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_INST_INLINER_H
