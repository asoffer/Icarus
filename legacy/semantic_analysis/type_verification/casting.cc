#include "semantic_analysis/type_verification/casting.h"

#include <type_traits>

#include "nth/debug/debug.h"

namespace semantic_analysis {

CastKind CanCast(QualifiedType from, core::Type to) {
  if (from.type() == to) { return CastKind::InPlace; }
  if (from.qualifiers() >= Qualifiers::Reference()) {
    if (auto p = to.get_if<core::PointerType>(GlobalTypeSystem);
        p and p->pointee() == from.type()) {
      return CastKind::InPlace;
    } else if (auto p = to.get_if<BufferPointerType>(GlobalTypeSystem);
               p and p->pointee() == from.type()) {
      return CastKind::InPlace;
    }
  }

  return GlobalTypeSystem.visit(from.type(), [&](auto from_type) -> CastKind {
    auto category_type = nth::type<std::decay_t<decltype(from_type)>>;
    if constexpr (category_type == nth::type<core::SizedIntegerType>) {
      if (to == Integer) {
        if (from.qualifiers() >= Qualifiers::Constant()) {
          return CastKind::Implicit;
        } else {
          return CastKind::None;
        }
      } else if (auto to_type =
                     to.get_if<core::SizedIntegerType>(GlobalTypeSystem)) {
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
    } else if constexpr (category_type == nth::type<PrimitiveType>) {
      switch (from_type.value()) {
        case Primitive::Bool:
        case Primitive::Char:
        case Primitive::Byte: return CastKind::None;
        case Primitive::F32:
          return to == F64 ? CastKind::Implicit : CastKind::None;
        case Primitive::F64: return CastKind::None;
        case Primitive::Integer:
          if (from.qualifiers() >= Qualifiers::Constant() and
              to.is<core::SizedIntegerType>(GlobalTypeSystem)) {
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
        case Primitive::NullPtr:
          return (to.is<core::PointerType>(GlobalTypeSystem) or
                  to.is<BufferPointerType>(GlobalTypeSystem))
                     ? CastKind::Implicit
                     : CastKind::None;
        case Primitive::EmptyArray: NTH_UNIMPLEMENTED();
        case Primitive::Type:
        case Primitive::Module:
        case Primitive::NoReturn:
        case Primitive::Error: return CastKind::None;
      }
    } else if constexpr (category_type == nth::type<BufferPointerType>) {
      if (auto to_ptr = to.get_if<BufferPointerType>(GlobalTypeSystem)) {
        if (CanCast(QualifiedType(from.type()
                                      .get<BufferPointerType>(GlobalTypeSystem)
                                      .pointee()),
                    to_ptr->pointee()) == CastKind::InPlace) {
          return CastKind::InPlace;
        } else {
          return CastKind::None;
        }
      }

      if (auto to_ptr = to.get_if<core::PointerType>(GlobalTypeSystem)) {
        if (CanCast(QualifiedType(from.type()
                                      .get<BufferPointerType>(GlobalTypeSystem)
                                      .pointee()),
                    to_ptr->pointee()) == CastKind::InPlace) {
          return CastKind::InPlace;
        } else {
          return CastKind::None;
        }
      }
    } else if constexpr (category_type == nth::type<core::PointerType>) {
      auto to_ptr = to.get_if<core::PointerType>(GlobalTypeSystem);
      if (to_ptr and
          CanCast(QualifiedType(from.type()
                                    .get<core::PointerType>(GlobalTypeSystem)
                                    .pointee()),
                  to_ptr->pointee()) == CastKind::InPlace) {
        return CastKind::InPlace;
      } else {
        return CastKind::None;
      }
    } else if constexpr (category_type == nth::type<SliceType>) {
      auto to_slice = to.get_if<SliceType>(GlobalTypeSystem);
      if (to_slice and
          CanCast(QualifiedType(
                      from.type().get<SliceType>(GlobalTypeSystem).pointee()),
                  to_slice->pointee()) == CastKind::InPlace) {
        return CastKind::InPlace;
      } else {
        return CastKind::None;
      }
    } else {
      NTH_UNIMPLEMENTED("{} -> {}") <<=
          {DebugQualifiedType(from), DebugType(to)};
    }
    return CastKind::None;
  });
}

core::Type CommonType(core::Type lhs, core::Type rhs) {
  if (lhs == rhs) { return lhs; }
  if (lhs == Integer) { return rhs; }
  if (rhs == Integer) { return lhs; }
  NTH_UNIMPLEMENTED("{} ~ {}") <<= {DebugType(lhs), DebugType(rhs)};
}

}  // namespace semantic_analysis
