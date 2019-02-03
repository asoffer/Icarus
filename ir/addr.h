#ifndef ICARUS_IR_ADDR_H
#define ICARUS_IR_ADDR_H

#include <iosfwd>

#include "base/types.h"
#include "base/untyped_buffer.h"

namespace ir {
struct Addr {
  enum class Kind : u8 { Heap, Stack, ReadOnly } kind;

  constexpr Addr() : kind(Kind::Heap), as_heap(nullptr) {}
  constexpr static Addr Null() { return Addr{}; }

  constexpr static Addr ReadOnly(u64 index) {
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

  constexpr static Addr Stack(u64 index) {
    Addr addr;
    addr.kind     = Kind::Stack;
    addr.as_stack = index;
    return addr;
  }

  union {
    u64 as_stack;
    void *as_heap;
    u64 as_rodata;
  };

  std::string to_string() const;
};

std::ostream &operator<<(std::ostream &os, Addr addr);

bool operator==(Addr lhs, Addr rhs);
inline bool operator!=(Addr lhs, Addr rhs) { return !(lhs == rhs); }

bool operator<(Addr lhs, Addr rhs);
inline bool operator<=(Addr lhs, Addr rhs) { return !(rhs < lhs); }
inline bool operator>(Addr lhs, Addr rhs) { return rhs < lhs; }
inline bool operator>=(Addr lhs, Addr rhs) { return !(lhs < rhs); }

}  // namespace ir

#endif  // ICARUS_IR_ADDR_H
