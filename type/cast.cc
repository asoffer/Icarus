#include "type/cast.h"

#include "common/integer.h"

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
  if (from.has_value()) {
    if (to.kind() == Type::Kind::Refinement) {
      if (to.AsRefinement().underlying() != from.type()) { return false; }
      return to.AsRefinement()(from);
    } else if (from.type() == Integer and to.kind() == Type::Kind::Primitive) {
      auto n = from.value()[0].as<::ic::Integer>();
      switch (to.AsPrimitive().kind()) {
        default: NTH_UNREACHABLE();
        case PrimitiveType::Kind::I8:
          return n >= std::numeric_limits<int8_t>::lowest() and
                 n <= std::numeric_limits<int8_t>::max();
        case PrimitiveType::Kind::U8:
          return true;
          // TODO: Right now this would break some c-headers that use
          // ascii_encode return n <= std::numeric_limits<uint8_t>::max();
        case PrimitiveType::Kind::I16:
          return n >= std::numeric_limits<int16_t>::lowest() and
                 n <= std::numeric_limits<int16_t>::max();
        case PrimitiveType::Kind::U16:
          return n <= std::numeric_limits<uint16_t>::max();
        case PrimitiveType::Kind::I32:
          return n >= std::numeric_limits<int32_t>::lowest() and
                 n <= std::numeric_limits<int32_t>::max();
        case PrimitiveType::Kind::U32:
          return n <= std::numeric_limits<uint32_t>::max();
        case PrimitiveType::Kind::I64:
          return n >= std::numeric_limits<int64_t>::lowest() and
                 n <= std::numeric_limits<int64_t>::max();
        case PrimitiveType::Kind::U64:
          return n <= std::numeric_limits<uint64_t>::max();
      }
    }
  }
  return ImplicitCast(from.type(), to);
}

}  // namespace ic::type
