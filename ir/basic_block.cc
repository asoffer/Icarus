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
  for (auto iter = b.cmd_buffer_.cbegin(); iter < b.cmd_buffer_.cend();) {
    auto cmd_index = iter.read<cmd_index_t>();
    switch (cmd_index) {
#define ICARUS_IR_CMD_X(type)                                                  \
  case type::index:                                                            \
    os << "    " #type " " << type::DebugString(&iter) << "\n";                \
    break;
#include "ir/cmd/cmd.xmacro.h"
#undef ICARUS_IR_CMD_X
      default: UNREACHABLE(static_cast<int>(cmd_index));
    }
  }
  os << b.jump_.DebugString() << "\n";
  return os;
}

BasicBlock::BasicBlock(BasicBlock const &b)
    : cmd_buffer_(b.cmd_buffer_), jump_(b.jump_) {
  AddOutgoingJumps(jump_);
}

BasicBlock::BasicBlock(BasicBlock &&b) noexcept
    : cmd_buffer_(std::move(b.cmd_buffer_)), jump_(std::move(b.jump_)) {
  ExchangeJumps(&b);
  b.jump_ = JumpCmd::Return();
}

BasicBlock &BasicBlock::operator=(BasicBlock const &b) noexcept {
  RemoveOutgoingJumps();
  AddOutgoingJumps(b.jump_);

  cmd_buffer_ = b.cmd_buffer_;
  jump_ = b.jump_;
  return *this;
}

void BasicBlock::AddOutgoingJumps(JumpCmd const &jump) {
  jump.Visit([&](auto const &j) {
    using type = std::decay_t<decltype(j)>;
    if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
      j.block->incoming_.insert(this);
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
      j.true_block->incoming_.insert(this);
      j.false_block->incoming_.insert(this);
    }
  });
}

void BasicBlock::RemoveOutgoingJumps() {
  jump_.Visit([&](auto const &j) {
    using type = std::decay_t<decltype(j)>;
    if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
      j.block->incoming_.erase(this);
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
      j.true_block->incoming_.erase(this);
      j.false_block->incoming_.erase(this);
    }
  });
}

void BasicBlock::ExchangeJumps(BasicBlock const *b) {
  b->jump_.Visit([&](auto const &j) {
    using type = std::decay_t<decltype(j)>;
    if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
      j.block->incoming_.insert(this);
      j.block->incoming_.erase(b);
    } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
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
  jump_       = std::exchange(b.jump_, JumpCmd::Return());
  cmd_buffer_ = std::move(b.cmd_buffer_);
  return *this;
}

void BasicBlock::Append(BasicBlock &&b) {
  ASSERT(jump_.kind() == JumpCmd::Kind::Uncond);
  cmd_buffer_.write(cmd_buffer_.size(), b.cmd_buffer_);
  jump_ = std::move(b.jump_);
}

Reg MakeResult(type::Type const *t) {
  auto arch = core::Interpretter();
  return Reserve(t->bytes(arch), t->alignment(arch));
}

BasicBlock const *ReturnBlock() {
  static BasicBlock b;
  return &b;
}
}  // namespace ir
