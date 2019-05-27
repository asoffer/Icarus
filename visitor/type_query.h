#ifndef ICARUS_VISITOR_TYPE_QUERY_H
#define ICARUS_VISITOR_TYPE_QUERY_H

#include "type/type_fwd.h"

namespace visitor {
// TODO: This doesn't even need to be a struct.
struct TypeQuery {
  static bool IsDefaultInitializable(type::Type const *) { return true; }
  static bool IsDefaultInitializable(type::Enum const *t);
  static bool IsDefaultInitializable(type::Opaque const *t);
  static bool IsDefaultInitializable(type::Struct const *t);
  static bool IsDefaultInitializable(type::Variant const *t);

  static bool IsCopyable(type::Type const *) { return true; }
  static bool IsCopyable(type::Array const *t);
  static bool IsCopyable(type::Struct const *t);
  static bool IsCopyable(type::Tuple const *t);
  static bool IsCopyable(type::Variant const *t);

  static bool IsMovable(type::Type const *) { return true; }
  static bool IsMovable(type::Array const *t);
  static bool IsMovable(type::Struct const *t);
  static bool IsMovable(type::Tuple const *t);
  static bool IsMovable(type::Variant const *t);

  static bool HasDestructor(type::Type const*) { return false; }
  static bool HasDestructor(type::Array const *t);
  static bool HasDestructor(type::Struct const *t);
  static bool HasDestructor(type::Tuple const *t);
  static bool HasDestructor(type::Variant const *t);

  static bool ReinterpretableAs(type::Type const *from, type::Type const *to) {
    return from == to;
  }
  static bool ReinterpretableAs(type::Array const *from,
                                type::Type const *to);
  static bool ReinterpretableAs(type::BufferPointer const *from,
                                type::Type const *to);
  static bool ReinterpretableAs(type::Pointer const *from,
                                type::Type const *to);
  static bool ReinterpretableAs(type::Primitive const *from,
                                type::Type const *to);
  static bool ReinterpretableAs(type::Tuple const *from,
                                type::Type const *to);
  static bool ReinterpretableAs(type::Variant const *from,
                                type::Type const *to);
};
}  // namespace visitor

#endif  // ICARUS_VISITOR_TYPE_QUERY_H