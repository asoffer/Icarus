#include "ir.h"

namespace IR {
void Block::ValidateCalls() {
  // TODO track data in registers
  for (auto& cmd : cmds_) {
    if (cmd.op_code == Op::Validate) { LOG << "Hi there"; }
  }
}

void Func::ValidateCalls() {
  // TODO track data across blocks
  for (auto &block : blocks_) { block.ValidateCalls(); }
}
} // namespace IR
