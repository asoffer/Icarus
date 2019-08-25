#include "ir/basic_block.h"

#include "ir/compiled_fn.h"
#include "type/type.h"

namespace ir {
thread_local BlockIndex BasicBlock::Current;

Reg Reserve(core::Bytes b, core::Alignment a) {
  return CompiledFn::Current->Reserve(b, a);
}
Reg Reserve(type::Type const *t) { return CompiledFn::Current->Reserve(t); }

std::ostream &operator<<(std::ostream &os, BasicBlock const &b) {
  return os << b.cmd_buffer_.to_string();
}

void BasicBlock::Append(BasicBlock &&b) { NOT_YET(); }

BasicBlock &GetBlock() {
  return ASSERT_NOT_NULL(CompiledFn::Current)->block(BasicBlock::Current);
}

Reg MakeResult(type::Type const *t) {
  auto arch = core::Interpretter();
  return Reserve(t->bytes(arch), t->alignment(arch));
}

}  // namespace ir
