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

namespace ir {
struct Inliner;
}  // namespace ir
namespace ir::internal {

// A `BlockGroup` represents a collection of `BasicBlock`s which make sense
// together as a coherent entity. One might normally call such a collection of
// `BasicBlock`s a function, but Icarus has at least one other useful example: A
// `JumpHandler`. The IR for a JumpHandler is largely similar to that of a
// function with a few differences. A `BlockGroup` represents the parts common
// to both.
struct BlockGroup {
  explicit BlockGroup(
      core::FnParams<type::Typed<ast::Declaration const *>> params);

  base::PtrSpan<BasicBlock const> blocks() const { return blocks_; }
  base::PtrSpan<BasicBlock> blocks() { return blocks_; }

  BasicBlock const *entry() const { return blocks()[0]; }
  BasicBlock *entry() { return blocks()[0]; }

  BasicBlock *AppendBlock() {
    return blocks_.emplace_back(std::make_unique<BasicBlock>(this)).get();
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
    return iter != reg_to_offset_.end() ? &iter->second : nullptr;
  }

 private:
  friend struct ::ir::Inliner; // TODO remove.
  core::FnParams<type::Typed<ast::Declaration const *>> params_;
  std::vector<std::unique_ptr<BasicBlock>> blocks_;
  StackFrameAllocations allocs_;

  // This vector is indexed by Reg and stores the value which is the offset
  // into the base::untyped_buffer holding all registers during compile-time
  // execution. It is only valid for core::Host().
  absl::flat_hash_map<Reg, core::Bytes> reg_to_offset_;

  // The size of an untyped_buffer required to construct
  core::Bytes reg_size_ = core::Bytes{0};
};

inline std::ostream &operator<<(std::ostream &os, BlockGroup const &b) {
  for (size_t i = 0; i < b.blocks().size(); ++i) {
    os << "\n block #" << i << "\n" << *b.blocks()[i];
  }
  return os;
}

}  // namespace ir::internal

#endif  // ICARUS_IR_BLOCK_GROUP_H
