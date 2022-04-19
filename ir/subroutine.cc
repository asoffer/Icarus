#include "ir/subroutine.h"

namespace ir {

Subroutine::Subroutine(type::Callable const *type)
    : type_(type), alloc_(parameters().size()) {
  AppendBlock(BasicBlock::DebugInfo{.header = "Entry"});
}

Reg Subroutine::Alloca(type::Type t) { return alloc_.StackAllocate(t); }
Reg Subroutine::Alloca(core::TypeContour tc) {
  return alloc_.StackAllocate(tc);
}

std::ostream &operator<<(std::ostream &os, Subroutine const &s) {
  os << "subroutine: " << s.type()->to_string() << "\n" << s.alloc_;
  for (size_t i = 0; i < s.blocks().size(); ++i) {
    os << "\n block #" << i << " (" << s.blocks()[i] << ")\n" << *s.blocks()[i];
  }
  return os;
}

}  // namespace ir
