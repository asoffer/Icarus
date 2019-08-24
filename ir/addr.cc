#include "ir/addr.h"

#include <string>

#include "base/debug.h"

namespace ir {

std::string Addr::to_string() const {
  std::stringstream ss;
  switch (kind) {
    case Kind::Stack: ss << "s." << as_stack; break;
    case Kind::Heap: ss << "h." << as_heap; break;
    case Kind::ReadOnly: ss << "ro." << as_rodata; break;
    default: UNREACHABLE(static_cast<int>(kind));
  }
  return ss.str();
}

bool operator==(Addr lhs, Addr rhs) {
  if (lhs.kind != rhs.kind) { return false; }
  switch (lhs.kind) {
    case Addr::Kind::Stack: return lhs.as_stack == rhs.as_stack;
    case Addr::Kind::Heap: return lhs.as_heap == rhs.as_heap;
    case Addr::Kind::ReadOnly: return lhs.as_rodata == rhs.as_rodata;
    default: UNREACHABLE(static_cast<int>(lhs.kind));
  }
}

std::string stringify(Addr::Kind k) {
  switch (k) {
    case Addr::Kind::Heap: return "heap";
    case Addr::Kind::Stack: return "stack";
    case Addr::Kind::ReadOnly: return "readonly";
    default: UNREACHABLE(static_cast<int>(k));
  }
}

Addr &Addr::operator+=(core::Bytes b) {
  switch (kind) {
    case Kind::Stack: as_stack += b.value(); break;
    case Kind::Heap: as_heap = static_cast<char *>(as_heap) + b.value(); break;
    case Kind::ReadOnly: as_rodata += b.value(); break;
  }
  return *this;
}

}  // namespace ir
