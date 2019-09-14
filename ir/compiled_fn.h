#ifndef ICARUS_IR_COMPILED_FN_H
#define ICARUS_IR_COMPILED_FN_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "base/move_func.h"
#include "base/scope.h"
#include "core/fn_params.h"
#include "ir/basic_block.h"
#include "ir/builder.h"
#include "ir/inliner.h"
#include "ir/stack_frame_allocations.h"
#include "type/typed_value.h"

namespace ast {
struct Expression;
}  // namespace ast

namespace type {
struct Function;
}  // namespace type

struct Module;

namespace ir {
struct BlockDef;

struct CmdIndex {
  BlockIndex block;
  int32_t cmd;
};

inline bool operator==(CmdIndex lhs, CmdIndex rhs) {
  return lhs.block == rhs.block && lhs.cmd == rhs.cmd;
}

inline bool operator<(CmdIndex lhs, CmdIndex rhs) {
  if (lhs.block.value < rhs.block.value) return true;
  if (lhs.block.value > rhs.block.value) return false;
  return lhs.cmd < rhs.cmd;
}

struct CompiledFn {

  CompiledFn(Module *mod, type::Function const *fn_type,
             core::FnParams<type::Typed<ast::Expression const *>> params);

  Inliner inliner() {
    BlockIndex index;
    index.value = static_cast<decltype(index.value)>(blocks_.size());
    blocks_.emplace_back(this);
    return Inliner(compiler_reg_to_offset_.size(), blocks_.size() - 1, index);
  }

  std::string name() const;

  BasicBlock const &block(BlockIndex index) const {
    return blocks_.at(index.value);
  }
  BasicBlock &block(BlockIndex index) {
    return const_cast<BasicBlock &>(
        static_cast<CompiledFn const *>(this)->block(index));
  }

  Reg Reserve(type::Type const *t);
  Reg Reserve(core::Bytes b, core::Alignment a);

  Reg Alloca(type::Type const* t);

  BlockIndex entry() const { return BlockIndex(0); }

  StackFrameAllocations const &allocs() { return allocs_; }

  type::Function const *const type_ = nullptr;
  core::FnParams<type::Typed<ast::Expression const *>> params_;

  int32_t num_regs_ = 0;
  std::vector<BasicBlock> blocks_;
  base::move_func<void()> *work_item = nullptr;

  Module *mod_;

  core::Bytes reg_size_ = core::Bytes{0};

  absl::flat_hash_map<Reg, CmdIndex> reg_to_cmd_;

  // This vector is indexed by Reg and stores the value which is the offset
  // into the base::untyped_buffer holding all registers during compile-time
  // execution. It is only valid for core::Host().
  absl::flat_hash_map<Reg, size_t> compiler_reg_to_offset_;
  StackFrameAllocations allocs_;
  bool must_inline_ = false;

  // TODO this is a hack until you figure out how to handle scoping/jump
  // handlers/etc. correctly. For now, jump_handlers fill this out and regular
  // functions do not. Probably this means these two things should be separated
  // into different types.
  std::vector<BlockDef const *> jumps_;
};

static_assert(alignof(CompiledFn) > 1);

std::ostream &operator<<(std::ostream &, CompiledFn const &);

}  // namespace ir

#endif  // ICARUS_IR_COMPILED_FN_H
