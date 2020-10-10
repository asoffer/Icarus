#include "ir/blocks/basic.h"

namespace ir {

std::ostream &operator<<(std::ostream &os, BasicBlock const &b) {
  os << " [with " << b.incoming().size() << " incoming]\n";
  for (auto const &inst : b.instructions_) {
    if (not inst) { continue; }
    os << "    " << inst.to_string() << '\n';
  }
  os << b.jump_.DebugString() << "\n";
  return os;
}

BasicBlock::BasicBlock(BasicBlock const &b) noexcept
    : instructions_(b.instructions_), jump_(b.jump_) {
  AddOutgoingJumps(jump_);
}

BasicBlock::BasicBlock(BasicBlock &&b) noexcept
    : instructions_(std::move(b.instructions_)), jump_(std::move(b.jump_)) {
  ExchangeJumps(&b);
  b.jump_ = JumpCmd::Return();
}

BasicBlock &BasicBlock::operator=(BasicBlock const &b) noexcept {
  RemoveOutgoingJumps();
  AddOutgoingJumps(b.jump_);
  instructions_ = b.instructions_;
  jump_         = b.jump_;
  return *this;
}

void BasicBlock::AddOutgoingJumps(JumpCmd const &jump) {
  jump.Visit([&](auto const &j) {
    constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
    if constexpr (type == base::meta<JumpCmd::UncondJump>) {
      LOG("AddOutgoingJumps", "Inserting %p into uncond-jump(%p)", this,
          j.block);

      j.block->incoming_.insert(this);
    } else if constexpr (type == base::meta<JumpCmd::CondJump>) {
      LOG("AddOutgoingJumps", "Inserting %p into cond-jump(%p, %p)", this,
          j.true_block, j.false_block);
      j.true_block->incoming_.insert(this);
      j.false_block->incoming_.insert(this);
    }
  });
}

void BasicBlock::RemoveOutgoingJumps() {
  jump_.Visit([&](auto const &j) {
    constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
    if constexpr (type == base::meta<JumpCmd::UncondJump>) {
      LOG("RemoveOutgoingJumps", "Removing %p from uncond-jump(%p)", this,
          j.block);
      j.block->incoming_.erase(this);
    } else if constexpr (type == base::meta<JumpCmd::CondJump>) {
      LOG("RemoveOutgoingJumps", "Removing %p from cond-jump(%p, %p)", this,
          j.true_block, j.false_block);
      j.true_block->incoming_.erase(this);
      j.false_block->incoming_.erase(this);
    }
  });
}

void BasicBlock::ExchangeJumps(BasicBlock const *b) {
  b->jump_.Visit([&](auto const &j) {
    constexpr auto type = base::meta<std::decay_t<decltype(j)>>;
    if constexpr (type == base::meta<JumpCmd::UncondJump>) {
      LOG("ExchangeJumps", "Inserting %p from uncond-jump", this);
      LOG("ExchangeJumps", "Removing %p from uncond-jump", b);
      j.block->incoming_.insert(this);
      j.block->incoming_.erase(b);
    } else if constexpr (type == base::meta<JumpCmd::CondJump>) {
      LOG("ExchangeJumps", "Inserting %p from cond-jump", this);
      LOG("ExchangeJumps", "Removing %p from cond-jump", b);
      j.true_block->incoming_.insert(this);
      j.true_block->incoming_.erase(b);
      j.false_block->incoming_.insert(this);
      j.false_block->incoming_.erase(b);
    }
  });
}

BasicBlock &BasicBlock::operator=(BasicBlock &&b) noexcept {
  RemoveOutgoingJumps();
  ExchangeJumps(&b);

  instructions_ = std::move(b.instructions_);
  jump_         = std::exchange(b.jump_, JumpCmd::Return());
  return *this;
}

void BasicBlock::Append(BasicBlock &&b) {
  ASSERT(jump_.kind() == JumpCmd::Kind::Uncond);
  RemoveOutgoingJumps();
  ExchangeJumps(&b);
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
      if (j.block == old_target) {
        j.block->incoming_.erase(this);
        j.block = new_target;
        j.block->incoming_.insert(this);
      }
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
      if (j.true_block == old_target) {
        j.true_block->incoming_.erase(this);
        j.true_block = new_target;
        j.true_block->incoming_.insert(this);
      }

      if (j.false_block == old_target) {
        j.false_block->incoming_.erase(this);
        j.false_block = new_target;
        j.false_block->incoming_.insert(this);
      }
    }
  });
}

}  // namespace ir
