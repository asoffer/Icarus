#include "type/cast.h"

namespace ic::type {

bool ImplicitCast(AnyValue const &from, Type to) {
  if (from.type() == to) { return true; }
  if (from.type() == Integer) {
    if (to.kind() == Type::Kind::Primitive and Numeric(to.AsPrimitive())) {
      return true;
    }
  } else if (from.type() == U8) {
    return to == I16 or to == I64 or to == I32 or to == U16 or to == U32 or
           to == U64;
  } else if (from.type() == U16) {
    return to == I32 or to == I64 or to == U32 or to == U64;
  } else if (from.type() == U32) {
    return to == I64 or to == U64;
  } else if (from.type() == NullType) {
    return to.kind() == Type::Kind::Pointer or
           to.kind() == Type::Kind::BufferPointer;
  }
  return false;
}

}  // namespace ic::type
