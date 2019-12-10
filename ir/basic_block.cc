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

void BasicBlock::Append(BasicBlock &&b) {
  cmd_buffer_.write(cmd_buffer_.size(), b.cmd_buffer_);
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
