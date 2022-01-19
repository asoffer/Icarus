#include "ir/basic_block.h"

namespace ir {

std::ostream &operator<<(std::ostream &os, BasicBlock const &b) {
  os << ' ' << b.debug().header << '\n';
  for (auto const &inst : b.instructions_) {
    if (not inst) { continue; }
    os << "  " << inst.to_string() << '\n';
  }
  os << b.jump_.DebugString() << '\n';
  return os;
}

BasicBlock::BasicBlock(BasicBlock const &b) noexcept
    : instructions_(b.instructions_), jump_(b.jump_), debug_(b.debug_) {}

BasicBlock::BasicBlock(BasicBlock &&b) noexcept
    : instructions_(std::move(b.instructions_)),
      jump_(std::move(b.jump_)),
      debug_(b.debug_) {
  b.jump_ = JumpCmd::Return();
}

BasicBlock &BasicBlock::operator=(BasicBlock const &b) noexcept {
  instructions_ = b.instructions_;
  jump_         = b.jump_;
  debug_        = b.debug_;
  return *this;
}

BasicBlock &BasicBlock::operator=(BasicBlock &&b) noexcept {
  instructions_ = std::move(b.instructions_);
  jump_         = std::exchange(b.jump_, JumpCmd::Return());
  return *this;
}

void BasicBlock::Append(BasicBlock &&b) {
  ASSERT(jump_.kind() == JumpCmd::Kind::Uncond);
  instructions_.insert(instructions_.end(),
                       std::make_move_iterator(b.instructions_.begin()),
                       std::make_move_iterator(b.instructions_.end()));
  b.instructions_.clear();
  jump_ = std::move(b.jump_);
}

void BasicBlock::ReplaceJumpTargets(BasicBlock *old_target,
                                    BasicBlock *new_target) {
  jump_.Visit([&](auto &j) {
    using type = std::decay_t<decltype(j)>;
    if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
      if (j.block == old_target) { j.block = new_target; }
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
      if (j.true_block == old_target) { j.true_block = new_target; }
      if (j.false_block == old_target) { j.false_block = new_target; }
    }
  });
}

}  // namespace ir
