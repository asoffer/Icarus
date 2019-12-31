#include "ir/addr.h"

#include <string>

#include "base/debug.h"

namespace ir {

std::string Addr::to_string() const {
  std::stringstream ss;
  switch (kind()) {
    case Kind::Stack: ss << "s." << stack(); break;
    case Kind::Heap: ss << "h." << heap(); break;
    case Kind::ReadOnly: ss << "ro." << rodata(); break;
    default: UNREACHABLE(static_cast<int>(kind()));
  }
  return ss.str();
}

std::string stringify(Addr::Kind k) {
  switch (k) {
    case Addr::Kind::Heap: return "heap";
    case Addr::Kind::Stack: return "stack";
    case Addr::Kind::ReadOnly: return "readonly";
    default: UNREACHABLE(static_cast<int>(k));
  }
}

}  // namespace ir
