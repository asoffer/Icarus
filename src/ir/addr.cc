#include "ir/addr.h"

#include <iosfwd>
#include <string>

#include "base/debug.h"

namespace ir {
bool operator<(Addr lhs, Addr rhs) {
  u8 lhs_kind = static_cast<u8>(lhs.kind);
  u8 rhs_kind = static_cast<u8>(rhs.kind);
  if (lhs_kind < rhs_kind) { return true; }
  if (lhs_kind > rhs_kind) { return false; }
  switch (lhs.kind) {
    case Addr::Kind::Stack: return lhs.as_stack < rhs.as_stack;
    case Addr::Kind::Heap: return lhs.as_heap < rhs.as_heap;
  }
  UNREACHABLE();
}

std::string Addr::to_string() const {
  std::stringstream ss;
  switch (kind) {
    case Kind::Stack: ss << "s." << as_stack; break;
    case Kind::Heap: ss << "h." << as_heap; break;
  }
  return ss.str();
}

bool operator==(Addr lhs, Addr rhs) {
  if (lhs.kind != rhs.kind) { return false; }
  switch (lhs.kind) {
    case Addr::Kind::Stack: return lhs.as_stack == rhs.as_stack;
    case Addr::Kind::Heap: return lhs.as_heap == rhs.as_heap;
  }
  UNREACHABLE();
}

std::ostream &operator<<(std::ostream &os, Addr addr) {
  return os << addr.to_string();
}

}  // namespace ir
