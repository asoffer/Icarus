#include "ir/basic_block.h"

#include "type/type.h"

namespace ir {

std::ostream &operator<<(std::ostream &os, BasicBlock const &b) { return os; }

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
