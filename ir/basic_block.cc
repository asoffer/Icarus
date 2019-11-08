#include "ir/basic_block.h"

#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "type/type.h"

namespace ir {
Reg Reserve(core::Bytes b, core::Alignment a) {
  return GetBuilder().CurrentGroup()->Reserve(b, a);
}

Reg Reserve(type::Type const *t) { return GetBuilder().CurrentGroup()->Reserve(t); }

std::ostream &operator<<(std::ostream &os, BasicBlock const &b) {
  return os << b.cmd_buffer_.to_string();
}

void BasicBlock::Append(BasicBlock &&b) { NOT_YET(); }

Reg MakeResult(type::Type const *t) {
  auto arch = core::Interpretter();
  return Reserve(t->bytes(arch), t->alignment(arch));
}

BasicBlock const *ReturnBlock() {
  static BasicBlock b;
  return &b;
}
}  // namespace ir
