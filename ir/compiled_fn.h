#ifndef ICARUS_IR_COMPILED_FN_H
#define ICARUS_IR_COMPILED_FN_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "base/bag.h"
#include "base/move_func.h"
#include "core/fn_params.h"
#include "ir/basic_block.h"
#include "ir/inliner.h"

namespace type {
struct Function;
}  // namespace type

struct Module;

namespace ir {
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
  static thread_local CompiledFn *Current;

  CompiledFn(Module *mod, type::Function const *fn_type,
             core::FnParams<type::Typed<ast::Expression const *>> params);

  Inliner inliner() {
    BlockIndex index;
    index.value = static_cast<decltype(index.value)>(blocks_.size());
    blocks_.emplace_back(this);
    return Inliner(compiler_reg_to_offset_.size(), blocks_.size(), index);
  }

  std::string name() const;

  BasicBlock const &block(BlockIndex index) const {
    ASSERT(blocks_.size() > static_cast<size_t>(index.value)) << Current;
    return blocks_.at(index.value);
  }
  BasicBlock &block(BlockIndex index) {
    return const_cast<BasicBlock &>(
        static_cast<CompiledFn const *>(this)->block(index));
  }

  Cmd const &Command(CmdIndex cmd_index) const {
    return *blocks_.at(cmd_index.block.value).cmds_.at(cmd_index.cmd);
  }

  Cmd const *Command(Reg reg) const;

  Cmd &Command(CmdIndex cmd_index) {
    return const_cast<Cmd &>(
        static_cast<CompiledFn const *>(this)->Command(cmd_index));
  }

  static BlockIndex AddBlock() {
    BlockIndex index;
    index.value = static_cast<decltype(index.value)>(Current->blocks_.size());
    Current->blocks_.emplace_back(Current);
    return index;
  }

  BlockIndex entry() const { return BlockIndex(0); }

  type::Function const *const type_ = nullptr;
  core::FnParams<type::Typed<ast::Expression const *>> params_;

  int32_t num_regs_ = 0;
  std::vector<BasicBlock> blocks_;
  base::move_func<void()> *work_item = nullptr;

  Module *mod_;

  core::Bytes reg_size_ = core::Bytes{0};

  absl::flat_hash_map<Reg, base::bag<Reg>> references_;
  absl::flat_hash_map<Reg, CmdIndex> reg_to_cmd_;

  // This vector is indexed by ir::Reg and stores the value which is the offset
  // into the base::untyped_buffer holding all registers during compile-time
  // execution. It is only valid for core::Host().
  absl::flat_hash_map<ir::Reg, size_t> compiler_reg_to_offset_;
  bool must_inline_ = false;

  // TODO this is a hack until you figure out how to handle scoping/jump
  // handlers/etc. correctly. For now, jump_handlers fill this out and regular
  // functions do not. Probably this means these two things should be separated
  // into different types.
  std::vector<ir::BlockDef const *> jumps_;
};

static_assert(alignof(CompiledFn) > 1);

std::ostream &operator<<(std::ostream &, ir::CompiledFn const &);

namespace internal {
struct FuncResetter {
  FuncResetter(CompiledFn *fn)
      : old_fn_(CompiledFn::Current), old_block_(BasicBlock::Current) {
    CompiledFn::Current = fn;
  }
  ~FuncResetter() {
    CompiledFn::Current = old_fn_;
    BasicBlock::Current = old_block_;
  }

  CompiledFn *old_fn_;
  BlockIndex old_block_;
};
}  // namespace internal
}  // namespace ir

#define CURRENT_FUNC(fn) if (ir::internal::FuncResetter resetter(fn); true)

#endif  // ICARUS_IR_COMPILED_FN_H
