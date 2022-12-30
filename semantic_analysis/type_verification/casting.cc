#include "semantic_analysis/type_verification/casting.h"

#include <type_traits>

#include "base/debug.h"
#include "base/log.h"

namespace semantic_analysis {

CastKind CanCast(QualifiedType from, core::Type to, TypeSystem& type_system) {
  if (from.type() == to) { return CastKind::InPlace; }

  return type_system.visit(from.type(), [&](auto from_type) -> CastKind {
    auto category_type = base::meta<std::decay_t<decltype(from_type)>>;
    if constexpr (category_type == base::meta<core::SizedIntegerType>) {
      if (to == Integer) {
        if (from.qualifiers() >= Qualifiers::Constant()) {
          return CastKind::Implicit;
        } else {
          return CastKind::None;
        }
      } else if (auto to_type =
                     to.get_if<core::SizedIntegerType>(type_system)) {
        if (to_type->is_signed()) {
          if (from_type.is_signed()) {
            // Signed -> signed conversion requires the bounds to be at least as
            // large on the output as the input.
            return from_type.bits() <= to_type->bits() ? CastKind::Implicit
                                                       : CastKind::None;
          } else {
            // Unsigned -> signed conversion requires the bounds to be strictly
            // larger on the output as the input.
            return from_type.bits() < to_type->bits() ? CastKind::Implicit
                                                      : CastKind::None;
          }
        } else {
          if (from_type.is_signed()) {
            // Signed -> unsigned casts are not possible.
            return CastKind::None;
          } else {
            // Unsigned -> unsigned conversion requires the bounds to at least
            // as large on the output as the input.
            return from_type.bits() <= to_type->bits() ? CastKind::Implicit
                                                       : CastKind::None;
          }
        }
      }

      LOG("", "%s", DebugType(from_type, type_system));

    } else if constexpr (category_type == base::meta<PrimitiveType>) {
      switch (from_type.value()) {
        case Primitive::Bool: NOT_YET();
        case Primitive::Char: NOT_YET();
        case Primitive::Byte: NOT_YET();
        case Primitive::F32:
          return to == F64 ? CastKind::Implicit : CastKind::None;
        case Primitive::F64: return CastKind::None;
        case Primitive::Integer:
          if (from.qualifiers() >= Qualifiers::Constant() and
              to.is<core::SizedIntegerType>(type_system)) {
            // Type-checking must additionally compute the value and determine
            // if the value is within range.
            return CastKind::Implicit;
          } else if (to == F32 or to == F64) {
            // Type-checking must additionally compute the value and determine
            // if the value is within range.
            return CastKind::Implicit;
          } else {
            return CastKind::None;
          }
        case Primitive::EmptyArray: NOT_YET();
        case Primitive::Type:
        case Primitive::Module:
        case Primitive::Error: return CastKind::None;
      }
    } else {
      NOT_YET(DebugQualifiedType(from, type_system), " -> ",
              DebugType(to, type_system));
    }
    return CastKind::None;
  });
}

}  // namespace semantic_analysis
