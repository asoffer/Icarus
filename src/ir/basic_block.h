#ifndef ICARUS_IR_BASIC_BLOCK_H
#define ICARUS_IR_BASIC_BLOCK_H
#include "base/container/vector.h"
#include "base/untyped_buffer.h"

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
  // TODO I don't need the whole vector to be ptr stable, but for now that's
  // easier. what I actually need is the data stable and knowledge of the length
  // (which I should have through other means)
  base::vector<std::unique_ptr<base::vector<IR::Val>>> call_args_;
  base::vector<std::unique_ptr<LongArgs>> long_args_;
};
}  // namespace IR
#endif  // ICARUS_IR_BASIC_BLOCK_H
