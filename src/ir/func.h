#ifndef ICARUS_IR_FUNC_H
#define ICARUS_IR_FUNC_H

#include <unordered_set>

#include "base/container/bag.h"
#include "base/container/unordered_map.h"
#include "base/container/vector.h"
#include "ir/basic_block.h"
#include "property/property_map.h"

#ifdef ICARUS_USE_LLVM
namespace llvm {
class Function;
}  // namespace llvm
#endif // ICARUS_USE_LLVM

namespace type {
struct Function;
}  // namespace type

struct Module;

namespace AST {
struct GeneratedFunction;
}  // namespace AST

namespace IR {
struct CmdIndex {
  BlockIndex block;
  i32 cmd;
};

inline bool operator==(CmdIndex lhs, CmdIndex rhs) {
  return lhs.block == rhs.block && lhs.cmd == rhs.cmd;
}

inline bool operator<(CmdIndex lhs, CmdIndex rhs) {
  if (lhs.block.value < rhs.block.value) return true;
  if (lhs.block.value > rhs.block.value) return false;
  return lhs.cmd < rhs.cmd;
}

struct Func {
  static thread_local Func *Current;

  Func(Module *mod, const type::Function *fn_type,
       base::vector<std::pair<std::string, AST::Expression *>> args);
  Func(Module *mod, AST::GeneratedFunction* fn_lit,
       base::vector<std::pair<std::string, AST::Expression *>> args);

  Register Argument(u32 n) const;
  Register Return(u32 n) const;

  void ComputeInvariants();
  void CheckInvariants();

  std::string name() const;

  const BasicBlock &block(BlockIndex index) const {
    ASSERT(blocks_.size() > static_cast<size_t>(index.value));
    return blocks_.at(index.value);
  }
  BasicBlock &block(BlockIndex index) {
    return const_cast<BasicBlock &>(
        static_cast<const Func *>(this)->block(index));
  }

  const Cmd &Command(CmdIndex cmd_index) const {
    return blocks_.at(cmd_index.block.value).cmds_.at(cmd_index.cmd);
  }

  Cmd const *Command(Register reg) const;

  Cmd &Command(CmdIndex cmd_index) {
    return const_cast<Cmd &>(
        static_cast<const Func *>(this)->Command(cmd_index));
  }

  static BlockIndex AddBlock() {
    BlockIndex index;
    index.value = static_cast<decltype(index.value)>(Current->blocks_.size());
    Current->blocks_.emplace_back(Current);
    return index;
  }

  BlockIndex entry() const { return BlockIndex(0); }

  // Is this needed? Or can it be determined from the containing
  // GeneratedFunction object?
  AST::GeneratedFunction *gened_fn_ = nullptr;
  const type::Function *const type_ = nullptr;
  base::vector<std::pair<std::string, AST::Expression *>> args_;
  bool has_default(size_t i) const { return args_[i].second != nullptr; }
  i32 num_regs_  = 0;
  i32 neg_bound_ = 0;
  base::vector<BasicBlock> blocks_;
#ifdef ICARUS_USE_LLVM
  llvm::Function *llvm_fn_ = nullptr;
#endif // ICARUS_USE_LLVM

  Module* mod_;

  size_t reg_size_ = 0;
  base::unordered_map<i32, Register> reg_map_;

  base::vector<AST::Expression *> precondition_exprs_, postcondition_exprs_;
  base::vector<std::pair<IR::Func, prop::PropertyMap>> preconditions_,
      postconditions_;
  base::unordered_map<Register, base::bag<Register>> references_;
  base::unordered_map<Register, CmdIndex> reg_to_cmd_;

  base::unordered_map<BasicBlock const *,
                      std::unordered_set<BasicBlock const *>>
  GetIncomingBlocks() const;
};
std::ostream &operator<<(std::ostream &, IR::Func const &);

namespace internal {
struct FuncResetter {
  FuncResetter(Func *fn) : old_fn_(Func::Current), old_block_(BasicBlock::Current) {
    Func::Current = fn;
  }
  ~FuncResetter() {
    Func::Current  = old_fn_;
    BasicBlock::Current = old_block_;
  }

  Func *old_fn_;
  BlockIndex old_block_;
};
} // namespace internal 
} // namespace IR

#define CURRENT_FUNC(fn) if (IR::internal::FuncResetter resetter(fn); true)

#endif // ICARUS_IR_FUNC_H
