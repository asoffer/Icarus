#ifndef ICARUS_IR_COMPILED_FN_H
#define ICARUS_IR_COMPILED_FN_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "base/bag.h"
#include "core/fn_params.h"
#include "ir/basic_block.h"
#include "property/property_map.h"

#ifdef ICARUS_USE_LLVM
namespace llvm {
class Function;
}  // namespace llvm
#endif  // ICARUS_USE_LLVM

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
             core::FnParams<type::Typed<ast::Expression *>> params);

  Reg Argument(uint32_t n) const;

  void ComputeInvariants();
  void CheckInvariants();

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
    return blocks_.at(cmd_index.block.value).cmds_.at(cmd_index.cmd);
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
  core::FnParams<type::Typed<ast::Expression *>> params_;

  int32_t num_regs_ = 0;
  std::vector<BasicBlock> blocks_;
  std::function<void()> *work_item = nullptr;
#ifdef ICARUS_USE_LLVM
  llvm::Function *llvm_fn_ = nullptr;
#endif  // ICARUS_USE_LLVM

  Module *mod_;

  core::Bytes reg_size_ = core::Bytes{0};

  std::vector<ast::Expression *> precondition_exprs_, postcondition_exprs_;
  std::vector<std::pair<ir::CompiledFn, prop::PropertyMap>> preconditions_,
      postconditions_;
  absl::flat_hash_map<Reg, base::bag<Reg>> references_;
  absl::flat_hash_map<Reg, CmdIndex> reg_to_cmd_;

  absl::flat_hash_map<BasicBlock const *,
                      absl::flat_hash_set<BasicBlock const *>>
  GetIncomingBlocks() const;

  // This vector is indexed by ir::Reg and stores the value which is the offset
  // into the base::untyped_buffer holding all registers during compile-time
  // execution. It is only valid for core::Host().
  std::vector<size_t> compiler_reg_to_offset_;
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
