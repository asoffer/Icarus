#ifndef ICARUS_IR_VALUE_BLOCK_H
#define ICARUS_IR_VALUE_BLOCK_H

#include <iostream>

namespace ir {
struct CompiledBlock;

struct Block {
  constexpr Block() : Block(nullptr) {}
  explicit constexpr Block(CompiledBlock *block) : block_(block) {}

  friend bool operator==(Block lhs, Block rhs) {
    return lhs.block_ == rhs.block_;
  }
  friend bool operator!=(Block lhs, Block rhs) { return not(lhs == rhs); }

  template <typename H>
  friend H AbslHashValue(H h, Block j) {
    return H::combine(std::move(h), j.block_);
  }

  friend std::ostream &operator<<(std::ostream &os, Block b) {
    return os << "Block(" << b.block_ << ")";
  }

 private:
  friend CompiledBlock;
  CompiledBlock *block_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_BLOCK_H
