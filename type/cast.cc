#include "type/cast.h"

namespace ic::type {

bool ImplicitCast(Type from, Type to) {
  if (from == to) { return true; }
  if (from == Integer) {
    if (to.kind() == Type::Kind::Primitive and Numeric(to.AsPrimitive())) {
      return true;
    }
  } else if (from == NullType) {
    return to.kind() == Type::Kind::Pointer or
           to.kind() == Type::Kind::BufferPointer;
  }
  return false;
}

}  // namespace ic::type
