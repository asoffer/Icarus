#ifndef ICARUS_IR_BLOCK_H
#define ICARUS_IR_BLOCK_H

namespace ast {
struct BlockLiteral;
}  // namespace ast

namespace ir {
struct Block {
  constexpr static Block Start() { return Block(nullptr); }
  constexpr static Block Exit() { return Block(0x1); }

  constexpr Block(ast::BlockLiteral const *lit)
      : data_(reinterpret_cast<uintptr_t>(lit)) {}

 private:
  constexpr Block(uintptr_t val) : data_(val){};
  uintptr_t data_;
};
}  // namespace ir

#endif  // ICARUS_IR_BLOCK_H
