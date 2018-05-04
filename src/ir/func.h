#ifndef ICARUS_IR_FUNC_H
#define ICARUS_IR_FUNC_H

#include <unordered_map>
#include <unordered_set>
#include <queue>
#include <vector>

#include "ir/basic_block.h"

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
struct FunctionLiteral;
}  // namespace AST


struct ExecContext;

namespace IR {
struct Func {
  static Func *Current;

  Func(Module *mod, const type::Function *fn_type,
       std::vector<std::pair<std::string, AST::Expression *>> args);
  Func(Module *mod, AST::FunctionLiteral* fn_lit,
       std::vector<std::pair<std::string, AST::Expression *>> args);

  void dump() const;
  Val Argument(u32 n);

  const std::string name() const {
    return std::to_string(reinterpret_cast<uintptr_t>(this));
  }

  int ValidateCalls(std::queue<Func *> *validation_queue) const;

  const BasicBlock &block(BlockIndex index) const { return blocks_.at(index.value); }
  BasicBlock &block(BlockIndex index) {
    return const_cast<BasicBlock &>(static_cast<const Func *>(this)->block(index));
  }

  const Cmd &Command(CmdIndex cmd_index) const {
    return blocks_.at(cmd_index.block.value).cmds_.at(cmd_index.cmd);
  }
  Cmd &Command(CmdIndex cmd_index) {
    return const_cast<Cmd &>(
        static_cast<const Func *>(this)->Command(cmd_index));
  }

  void SetArgs(CmdIndex cmd_index, std::vector<IR::Val> args);

  static BlockIndex AddBlock() {
    BlockIndex index;
    index.value = static_cast<decltype(index.value)>(Current->blocks_.size());
    Current->blocks_.emplace_back(Current);
    return index;
  }

  BlockIndex entry() const { return BlockIndex(0); }

  // Is this needed? Or can it be determined from the containing FunctionLiteral
  // object?
  AST::FunctionLiteral *fn_lit_     = nullptr;
  const type::Function *const type_ = nullptr;
  std::vector<std::pair<std::string, AST::Expression *>> args_;
  bool has_default(size_t i) const { return args_[i].second != nullptr; }
  i32 num_regs_  = 0;
  i32 num_voids_ = 0;
  std::vector<BasicBlock> blocks_;
#ifdef ICARUS_USE_LLVM
  llvm::Function *llvm_fn_ = nullptr;
#endif // ICARUS_USE_LLVM

  // Indices for blocks that end in a return statement.
  std::unordered_set<BlockIndex> return_blocks_;

  // TODO we can probably come up with a way to more closely tie Register and
  // CmdIndex so we don't need to store this map:
  std::unordered_map<Register, CmdIndex> reg_map_;
  std::vector<AST::Expression *> preconditions_;
  std::vector<AST::Expression *> postconditions_;
  Module* mod_;
  // TODO many of these maps could and should be vectors except they're keyed on
  // strong ints. Consider adding a strong int vector.
  std::unordered_map<CmdIndex, std::vector<CmdIndex>> references_;

  mutable int num_errors_ = -1; // -1 indicates not yet validated

  std::unordered_map<const BasicBlock *, std::unordered_set<const BasicBlock *>>
  GetIncomingBlocks() const;
};

template <bool B> BlockIndex EarlyExitOn(BlockIndex exit_block, Val cond) {
  auto continue_block = Func::Current->AddBlock();
  if constexpr (B) {
    CondJump(cond, exit_block, continue_block);
  } else {
    CondJump(cond, continue_block, exit_block);
  }
  return continue_block;
}

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
