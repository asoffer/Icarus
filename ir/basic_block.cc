#include "ir/basic_block.h"

#include "ir/cmd/basic.h"
#include "ir/cmd/call.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/phi.h"
#include "ir/cmd/print.h"
#include "ir/cmd/register.h"
#include "ir/cmd/return.h"
#include "ir/cmd/scope.h"
#include "ir/cmd/store.h"
#include "ir/cmd/types.h"
#include "type/type.h"

namespace ir {

std::ostream &operator<<(std::ostream &os, BasicBlock const &b) {
  os << " [with " << b.num_incoming() << " incoming; " << b.cmd_buffer_.size()
     << "]\n";
  for (auto const &inst : b.instructions_) { os << "    " << inst->to_string() << '\n'; }
  os << b.jump_.DebugString() << "\n";
  return os;
}

BasicBlock::BasicBlock(BasicBlock const &b)
    : cmd_buffer_(b.cmd_buffer_), jump_(b.jump_) {
  instructions_.reserve(b.instructions_.size());
  for (auto const &inst : b.instructions_) {
    instructions_.push_back(inst->clone());
  }
  AddOutgoingJumps(jump_);
}

BasicBlock::BasicBlock(BasicBlock &&b) noexcept
    : instructions_(std::move(b.instructions_)),
      cmd_buffer_(std::move(b.cmd_buffer_)),
      jump_(std::move(b.jump_)) {
  ExchangeJumps(&b);
  b.jump_ = JumpCmd::Return();
}

BasicBlock &BasicBlock::operator=(BasicBlock const &b) noexcept {
  RemoveOutgoingJumps();
  AddOutgoingJumps(b.jump_);

  instructions_.clear();
  instructions_.reserve(b.instructions_.size());
  for (auto const &inst : b.instructions_) {
    instructions_.push_back(inst->clone());
  }
  cmd_buffer_ = b.cmd_buffer_;
  jump_ = b.jump_;
  return *this;
}

void BasicBlock::AddOutgoingJumps(JumpCmd const &jump) {
  jump.Visit([&](auto const &j) {
    using type = std::decay_t<decltype(j)>;
    if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
      DEBUG_LOG("AddOutgoingJumps")
      ("Inserting ", this, " into uncond-jump(", j.block, ")");

      j.block->incoming_.insert(this);
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
      DEBUG_LOG("AddOutgoingJumps")
      ("Inserting ", this, " into cond-jump(", j.true_block, ", ",
       j.false_block, ")");
      j.true_block->incoming_.insert(this);
      j.false_block->incoming_.insert(this);
    }
  });
}

void BasicBlock::RemoveOutgoingJumps() {
  jump_.Visit([&](auto const &j) {
    using type = std::decay_t<decltype(j)>;
    if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
      DEBUG_LOG("RemoveOutgoingJumps")
      ("Removing ", this, " from uncond-jump(", j.block, ")");
      j.block->incoming_.erase(this);
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
      DEBUG_LOG("RemoveOutgoingJumps")
      ("Removing ", this, " from cond-jump(", j.true_block, ", ", j.false_block,
       ")");
      j.true_block->incoming_.erase(this);
      j.false_block->incoming_.erase(this);
    }
  });
}

void BasicBlock::ExchangeJumps(BasicBlock const *b) {
  b->jump_.Visit([&](auto const &j) {
    using type = std::decay_t<decltype(j)>;
    if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
      DEBUG_LOG("ExchangeJumps")("Inserting", this, " from uncond-jump");
      DEBUG_LOG("ExchangeJumps")("Removing ", b, " from uncond-jump");
      j.block->incoming_.insert(this);
      j.block->incoming_.erase(b);
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
      DEBUG_LOG("ExchangeJumps")("Inserting", this, " from cond-jump");
      DEBUG_LOG("ExchangeJumps")("Removing ", b, " from cond-jump");
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
  cmd_buffer_   = std::move(b.cmd_buffer_);
  return *this;
}

void BasicBlock::Append(BasicBlock const &b) {
  ASSERT(jump_.kind() == JumpCmd::Kind::Uncond);
  RemoveOutgoingJumps();
  ExchangeJumps(&b);
  cmd_buffer_.write(cmd_buffer_.size(), b.cmd_buffer_);
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
