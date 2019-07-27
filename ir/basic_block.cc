#include "ir/basic_block.h"

namespace ir {

std::ostream &operator<<(std::ostream &os, BasicBlock const &b) {
  for (const auto &cmd : b.cmds_) { os << "  " << *cmd << "\n"; }
  return os;
}

void BasicBlock::Append(BasicBlock &&b) {
  ASSERT(cmds_.empty() == false);
  // ASSERT(cmds_.back()->op_code_ == ir::Op::UncondJump);
  cmds_.pop_back();
  cmds_.insert(cmds_.end(), std::make_move_iterator(b.cmds_.begin()),
               std::make_move_iterator(b.cmds_.end()));
  b.cmds_.clear();
  arguments_.splice(arguments_.end(), b.arguments_);
  outs_.splice(outs_.end(), b.outs_);
  phi_args_.insert(phi_args_.end(),
                   std::make_move_iterator(b.phi_args_.begin()),
                   std::make_move_iterator(b.phi_args_.end()));
}

}  // namespace ir
