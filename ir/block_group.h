#ifndef ICARUS_IR_BLOCK_GROUP_H
#define ICARUS_IR_BLOCK_GROUP_H

#include <iostream>
#include <memory>
#include <vector>

#include "ast/ast_fwd.h"
#include "base/ptr_span.h"
#include "base/strong_types.h"
#include "core/alignment.h"
#include "core/bytes.h"
#include "core/fn_params.h"
#include "ir/basic_block.h"
#include "ir/reg.h"
#include "ir/stack_frame_allocations.h"
#include "type/type_fwd.h"
#include "type/typed_value.h"

namespace ir::internal {

// A `BlockGroup` represents a collection of `BasicBlock`s which make sense
// together as a coherent entity. One might normally call such a collection of
// `BasicBlock`s a function, but Icarus has at least one other useful example: A
// `Jump`. The IR for a Jump is largely similar to that of a
// function with a few differences. A `BlockGroup` represents the parts common
// to both.
struct BlockGroup {
  explicit BlockGroup(
      core::FnParams<type::Typed<ast::Declaration const *>> params);

  base::PtrSpan<BasicBlock const> blocks() const { return blocks_; }
  base::PtrSpan<BasicBlock> blocks() { return blocks_; }
  auto &mutable_blocks() { return blocks_; }

  BasicBlock const *entry() const { return blocks()[0]; }
  BasicBlock *entry() { return blocks()[0]; }

  BasicBlock *AppendBlock() {
    return blocks_.emplace_back(std::make_unique<BasicBlock>(this)).get();
  }

  BasicBlock *AppendBlock(BasicBlock const &to_copy) {
    auto *b = blocks_.emplace_back(std::make_unique<BasicBlock>(to_copy)).get();
    b->group_ = this;
    return b;
  }

  core::FnParams<type::Typed<ast::Declaration const *>> const &params() const {
    return params_;
  }

  void Serialize() {
    for (auto &block : blocks_) { block->Serialize(); }
  }

  StackFrameAllocations const &allocs() { return allocs_; }

  Reg Reserve() { return Reg(num_regs_++); }
  Reg Alloca(type::Type const *t);

  constexpr size_t num_regs() const { return num_regs_; }
  constexpr size_t num_args() const { return num_args_; }

 private:
  core::FnParams<type::Typed<ast::Declaration const *>> params_;
  std::vector<std::unique_ptr<BasicBlock>> blocks_;
  StackFrameAllocations allocs_;

  size_t num_regs_ = 0;
  size_t num_args_ = 0;
};

std::ostream &operator<<(std::ostream &os, BlockGroup const &b);

}  // namespace ir::internal

#endif  // ICARUS_IR_BLOCK_GROUP_H
