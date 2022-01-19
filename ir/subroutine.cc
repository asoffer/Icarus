#include "ir/subroutine.h"

namespace ir {

Subroutine::Subroutine(type::Callable const *type)
    : type_(type), alloc_(params().size()) {
  AppendBlock(BasicBlock::DebugInfo{.header = "Entry"});
}

Reg Subroutine::Alloca(type::Type t) { return alloc_.StackAllocate(t); }
Reg Subroutine::Alloca(core::TypeContour tc) {
  return alloc_.StackAllocate(tc);
}

std::ostream &operator<<(std::ostream &os, Subroutine const &b) {
  os << "\n" << b.alloc_;
  for (size_t i = 0; i < b.blocks().size(); ++i) {
    os << "\n block #" << i << " (" << b.blocks()[i] << ")\n" << *b.blocks()[i];
  }
  return os;
}

}  // namespace ir
