#ifndef ICARUS_IR_BASIC_BLOCK_H
#define ICARUS_IR_BASIC_BLOCK_H

#include <iostream>
#include <list>
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

  BasicBlock(BasicBlock const &b)
      : fn_(b.fn_),
        cmd_buffer_(b.cmd_buffer_),
        arguments_(b.arguments_),
        outs_(b.outs_) {
    cmds_.reserve(b.cmds_.size());
    for (size_t i = 0; i < cmds_.size(); ++i) {
      cmds_.push_back(std::make_unique<Cmd>(*b.cmds_[i]));
    }
    phi_args_.reserve(b.phi_args_.size());
    for (size_t i = 0; i < phi_args_.size(); ++i) {
      NOT_YET();
      // phi_args_.push_back(std::make_unique<GenericPhiArgs>(*b.phi_args_[i]));
    }
  }

  BasicBlock(BasicBlock &&) noexcept = default;

  BasicBlock &operator=(BasicBlock &&) noexcept = default;
  BasicBlock &operator=(BasicBlock const &b) { return *this = BasicBlock(b); }

  void Append(BasicBlock &&b);

  CompiledFn *fn_;  // Containing function
  std::vector<std::unique_ptr<Cmd>> cmds_;
  CmdBuffer cmd_buffer_;

  // These containers are append-only and we separately store pointers to these
  // elments so we never traverse. We just need pointer stabiltiy. In the long
  // term a single allocation is probably better but that's not easy with the
  // current setup.
  std::list<Arguments> arguments_;
  std::list<OutParams> outs_;
  std::vector<std::unique_ptr<GenericPhiArgs>> phi_args_;
};

BasicBlock &GetBlock();

template <typename T>
Reg MakeResult() {
  return Reserve(core::Bytes::Get<T>(), core::Alignment::Get<T>());
}

std::ostream &operator<<(std::ostream &os, BasicBlock const &b);

}  // namespace ir
#endif  // ICARUS_IR_BASIC_BLOCK_H
