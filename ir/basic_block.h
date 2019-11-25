#ifndef ICARUS_IR_BASIC_BLOCK_H
#define ICARUS_IR_BASIC_BLOCK_H

#include <iostream>
#include <memory>
#include <vector>

#include "base/untyped_buffer.h"
#include "core/alignment.h"
#include "core/bytes.h"
#include "ir/cmd_buffer.h"
#include "ir/out_params.h"

namespace ir {
namespace internal {
struct BlockGroup;
}  // namespace internal

struct BasicBlock {
  BasicBlock() = default;
  explicit BasicBlock(internal::BlockGroup *group) : group_(group) {}

  BasicBlock(BasicBlock const &b)    = default;
  BasicBlock(BasicBlock &&) noexcept = default;
  BasicBlock &operator=(BasicBlock &&) noexcept = default;
  BasicBlock &operator=(BasicBlock const &b) = default;

  void Append(BasicBlock &&b);

  CmdBuffer cmd_buffer_;

 private:
  internal::BlockGroup *group_;
};

BasicBlock const *ReturnBlock();

Reg Reserve(core::Bytes b, core::Alignment a);

template <typename T>
Reg MakeResult() {
  return Reserve(core::Bytes::Get<T>(), core::Alignment::Get<T>());
}

Reg MakeResult(type::Type const *t);

std::ostream &operator<<(std::ostream &os, BasicBlock const &b);

}  // namespace ir
#endif  // ICARUS_IR_BASIC_BLOCK_H
