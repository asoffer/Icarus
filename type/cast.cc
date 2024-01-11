#include "type/cast.h"

namespace ic::type {
namespace {

bool ImplicitCast(Type from, Type to) {
  if (from == to) { return true; }
  if (from == Integer) {
    if (to.kind() == Type::Kind::Primitive and Numeric(to.AsPrimitive())) {
      return true;
    }
  } else if (from == U8) {
    return to == I16 or to == I64 or to == I32 or to == U16 or to == U32 or
           to == U64;
  } else if (from == U16) {
    return to == I32 or to == I64 or to == U32 or to == U64;
  } else if (from == U32) {
    return to == I64 or to == U64;
  } else if (from == NullType) {
    return to.kind() == Type::Kind::Pointer or
           to.kind() == Type::Kind::BufferPointer;
  }
  return false;
}

}  // namespace

bool ImplicitCast(AnyValue const &from, Type to) {
  if (not from.has_value()) { return ImplicitCast(from.type(), to); }
  if (to.kind() == Type::Kind::Refinement) {
    if (to.AsRefinement().underlying() != from.type()) { return false; }
    return to.AsRefinement()(from);
  }
  return false;
}

}  // namespace ic::type
