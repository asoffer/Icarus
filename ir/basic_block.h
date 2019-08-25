#ifndef ICARUS_IR_BASIC_BLOCK_H
#define ICARUS_IR_BASIC_BLOCK_H

#include <iostream>
#include <memory>
#include <vector>

#include "base/untyped_buffer.h"
#include "ir/arguments.h"
#include "ir/cmd.h"
#include "ir/cmd_buffer.h"
#include "ir/out_params.h"

namespace ir {
struct CompiledFn;

struct BasicBlock {
  static thread_local BlockIndex Current;
  BasicBlock() = default;
  explicit BasicBlock(CompiledFn *fn) : fn_(fn) {}

  BasicBlock(BasicBlock const &b) : fn_(b.fn_), cmd_buffer_(b.cmd_buffer_) {
    cmds_.reserve(b.cmds_.size());
    for (size_t i = 0; i < cmds_.size(); ++i) {
      cmds_.push_back(std::make_unique<Cmd>(*b.cmds_[i]));
    }
  }

  BasicBlock(BasicBlock &&) noexcept = default;

  BasicBlock &operator=(BasicBlock &&) noexcept = default;
  BasicBlock &operator=(BasicBlock const &b) { return *this = BasicBlock(b); }

  void Append(BasicBlock &&b);

  CompiledFn *fn_;  // Containing function
  std::vector<std::unique_ptr<Cmd>> cmds_;
  CmdBuffer cmd_buffer_;
};

BasicBlock &GetBlock();

template <typename T>
Reg MakeResult() {
  return Reserve(core::Bytes::Get<T>(), core::Alignment::Get<T>());
}

inline Reg MakeResult(type::Type const *t) {
  auto arch = core::Interpretter();
  return Reserve(t->bytes(arch), t->alignment(arch));
}

std::ostream &operator<<(std::ostream &os, BasicBlock const &b);

}  // namespace ir
#endif  // ICARUS_IR_BASIC_BLOCK_H
