#ifndef ICARUS_IR_INSTRUCTION_INST_INLINER_H
#define ICARUS_IR_INSTRUCTION_INST_INLINER_H

#include <string_view>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "base/debug.h"
#include "base/meta.h"
#include "ir/blocks/basic.h"
#include "ir/blocks/group.h"
#include "ir/instruction/jump.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace ir {
// TODO: Remove forward-declaration.
struct BasicBlock;

// Note: `blocks_by_name_` is specific to inlining jumps.
struct InstructionInliner {
  explicit InstructionInliner(
      internal::BlockGroupBase const *to_be_inlined,
      internal::BlockGroupBase *into,
      absl::flat_hash_map<std::string_view, BasicBlock *> blocks_by_name);

  void Inline(BasicBlock *&block, BasicBlock *incoming_block) const;

  void InlineAllBlocks();

  absl::flat_hash_map<std::string_view,
                      std::vector<std::pair<Arguments, BasicBlock *>>>
  ArgumentsByName() && {
    return std::move(arguments_by_name_);
  }

  void SetAndInlineArguments(JumpCmd::JumpExitJump const &jump,
                             BasicBlock *block) {
    Arguments args = *ASSERT_NOT_NULL(choose_argument_cache_.at(jump.choose_block));
    base::Traverse(inliner_, args);
    arguments_by_name_[jump.name].emplace_back(std::move(args), block);
  }

 private:
  void FindBlocksToInline();

  // Returns the block associated with this name if it exists, and a null
  // pointer otherwise.
  BasicBlock *block_for(std::string_view name) const;

  // If a block exists that is associated with a name in the span, a pair is
  // returned consisting of the index into the span of that name  and the
  // corresponding block pointer. If no such block exists, a pair is returned
  // that has a null pointer for its second element. The value of the first
  // elemement is unspecified.
  std::pair<size_t, BasicBlock *> block_for(
      absl::Span<std::string_view const> names) const;

  void InlineJumpOnBlock(BasicBlock *block);

  internal::BlockGroupBase const *to_be_inlined_;
  internal::BlockGroupBase *into_;
  int register_offset_;
  absl::flat_hash_map<BasicBlock const *, BasicBlock *> to_rewrite_;
  absl::flat_hash_map<std::string_view,
                      std::vector<std::pair<Arguments, BasicBlock *>>>
      arguments_by_name_;

  // For each block ending in a ChooseJump, stash a pointer to the arguments
  // that would get chosen in this context.
  absl::flat_hash_map<BasicBlock const *, Arguments const *>
      choose_argument_cache_;
  absl::flat_hash_map<std::string_view, BasicBlock *> blocks_by_name_;
  Inliner inliner_;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_INST_INLINER_H
