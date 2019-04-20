#ifndef ICARUS_IR_BLOCK_H
#define ICARUS_IR_BLOCK_H

#include <iostream>

#include "base/debug.h"

namespace ast {
struct BlockLiteral;
}  // namespace ast

namespace ir {
struct Block {
  constexpr Block() = default;
  constexpr static Block Start() { return Block(nullptr); }
  constexpr static Block Exit() { return Block(0x1); }

  constexpr Block(ast::BlockLiteral const *lit)
      : data_(reinterpret_cast<uintptr_t>(lit)) {}

  friend std::ostream &operator<<(std::ostream &os, Block b) {
    return os << "block(" << b.data_ << ")";
  }
  friend constexpr bool operator==(Block lhs, Block rhs) {
    return lhs.data_ == rhs.data_;
  }

  ast::BlockLiteral const *get() {
    ASSERT(*this != Start());
    ASSERT(*this != Exit());
    return reinterpret_cast<ast::BlockLiteral const *>(data_);
  }

 private:
  constexpr Block(uintptr_t val) : data_(val){};
  uintptr_t data_ = 0;
};

constexpr bool operator!=(Block lhs, Block rhs) { return !(lhs == rhs); }

}  // namespace ir

#endif  // ICARUS_IR_BLOCK_H
