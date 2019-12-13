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
    return blocks_.emplace_back(std::make_unique<BasicBlock>(to_copy)).get();
  }

  core::FnParams<type::Typed<ast::Declaration const *>> const &params() const {
    return params_;
  }

  base::untyped_buffer MakeBuffer() const {
    return base::untyped_buffer::MakeFull(reg_size_.value());
  }

  StackFrameAllocations const &allocs() { return allocs_; }

  Reg Reserve(type::Type const *t);
  Reg Reserve(core::Bytes b, core::Alignment a);
  void Reserve(Reg r, core::Bytes b, core::Alignment a);
  Reg Alloca(type::Type const *t);

  core::Bytes const *offset_or_null(Reg r) const {
    auto iter = reg_to_offset_.find(r);
    DEBUG_LOG("offset_or_null")(reg_to_offset_);
    DEBUG_LOG("offset_or_null")
    ("offset_or_null(", r,
     ") = ", iter != reg_to_offset_.end() ? iter->second : core::Bytes(-1));
    return iter != reg_to_offset_.end() ? &iter->second : nullptr;
  }

  core::Bytes offset(Reg r) const {
#if defined(ICARUS_DEBUG)
    return ASSERT_NOT_NULL(offset_or_null(r));
#else
    return reg_to_offset_.find(r)->second;
#endif
  }

 private:
  core::FnParams<type::Typed<ast::Declaration const *>> params_;
  std::vector<std::unique_ptr<BasicBlock>> blocks_;
  StackFrameAllocations allocs_;

 public: // TODO remove publicity
  // This vector is indexed by Reg and stores the value which is the offset
  // into the base::untyped_buffer holding all registers during compile-time
  // execution. It is only valid for core::Host().
  absl::flat_hash_map<Reg, core::Bytes> reg_to_offset_;

 private:
  // The size of an untyped_buffer required to construct
  core::Bytes reg_size_ = core::Bytes{0};
};

std::ostream &operator<<(std::ostream &os, BlockGroup const &b);

}  // namespace ir::internal

#endif  // ICARUS_IR_BLOCK_GROUP_H
