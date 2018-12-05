#ifndef ICARUS_IR_ADDR_H
#define ICARUS_IR_ADDR_H

#include <iosfwd>

#include "base/types.h"

namespace ir {
struct Addr {
  enum class Kind : u8 { Heap, Stack } kind;

  constexpr Addr() : kind(Kind::Heap), as_heap(nullptr) {}
  constexpr static Addr Null() { return Addr{}; }

  union {
    u64 as_stack;
    void *as_heap;
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
