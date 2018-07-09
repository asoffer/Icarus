#ifndef ICARUS_IR_BASIC_BLOCK_H
#define ICARUS_IR_BASIC_BLOCK_H
#include "base/container/vector.h"

#include "cmd.h"

namespace IR {
struct Func;

struct BasicBlock {
  static BlockIndex Current;
  BasicBlock()                    = delete;
  BasicBlock(const BasicBlock &&) = delete;
  BasicBlock(BasicBlock &&)       = default;
  BasicBlock(Func *fn) : fn_(fn) {}

  BasicBlock &operator=(BasicBlock &&) = default;
  BasicBlock &operator=(const BasicBlock &) = delete;

  void dump(size_t indent) const;

  Func *fn_;  // Containing function
  base::vector<BlockIndex> incoming_blocks_;
  base::vector<Cmd> cmds_;
};
}  // namespace IR
#endif  // ICARUS_IR_BASIC_BLOCK_H
