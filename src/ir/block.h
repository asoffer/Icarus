#ifndef ICARUS_IR_BLOCK_H
#define ICARUS_IR_BLOCK_H
#include <vector>

#include "cmd.h"

namespace IR {
struct Func;

struct Block {
  static BlockIndex Current;
  Block()               = delete;
  Block(const Block &&) = delete;
  Block(Block &&)       = default;
  Block(Func *fn) : fn_(fn) {}

  Block &operator=(Block &&) = default;
  Block &operator=(const Block &) = delete;

  void dump(size_t indent) const;

  Func *fn_; // Containing function
  std::vector<BlockIndex> incoming_blocks_;
  std::vector<Cmd> cmds_;
};
} // namespace IR
#endif // ICARUS_IR_BLOCK_H
