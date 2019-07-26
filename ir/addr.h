#ifndef ICARUS_IR_ADDR_H
#define ICARUS_IR_ADDR_H

#include <string>

namespace ir {
struct Addr {
  enum class Kind : uint8_t { Heap, Stack, ReadOnly } kind;

  constexpr Addr() : kind(Kind::Heap), as_heap(nullptr) {}
  constexpr static Addr Null() { return Addr{}; }

  constexpr static Addr ReadOnly(uint64_t index) {
    Addr addr;
    addr.kind      = Kind::ReadOnly;
    addr.as_rodata = index;
    return addr;
  }

  constexpr static Addr Heap(void *ptr) {
    Addr addr;
    addr.kind    = Kind::Heap;
    addr.as_heap = ptr;
    return addr;
  }

  constexpr static Addr Stack(uint64_t index) {
    Addr addr;
    addr.kind     = Kind::Stack;
    addr.as_stack = index;
    return addr;
  }

  union {
    uint64_t as_stack;
    void *as_heap;
    uint64_t as_rodata;
  };

  std::string to_string() const;
};

bool operator==(Addr lhs, Addr rhs);
inline bool operator!=(Addr lhs, Addr rhs) { return !(lhs == rhs); }

}  // namespace ir

#endif  // ICARUS_IR_ADDR_H
