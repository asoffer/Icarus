#include "ir/basic_block.h"

namespace ir {

std::ostream &operator<<(std::ostream &os, BasicBlock const &b) {
  return os << b.cmd_buffer_.to_string();
}

void BasicBlock::Append(BasicBlock &&b) {
  ASSERT(cmds_.empty() == false);
  // ASSERT(cmds_.back()->op_code_ == ir::Op::UncondJump);
  cmds_.pop_back();
  cmds_.insert(cmds_.end(), std::make_move_iterator(b.cmds_.begin()),
               std::make_move_iterator(b.cmds_.end()));
  b.cmds_.clear();
}

}  // namespace ir
