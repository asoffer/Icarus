#ifndef ICARUS_SEMANTIC_ANALYSIS_TYPE_SYSTEM_H
#define ICARUS_SEMANTIC_ANALYSIS_TYPE_SYSTEM_H

#include "core/type_system/finite_set.h"
#include "core/type_system/function.h"
#include "core/type_system/parameter.h"
#include "core/type_system/pointer.h"
#include "core/type_system/sized_integer.h"
#include "core/type_system/type_system.h"

namespace semantic_analysis {

// `Qualifiers` represent extra information that may be relevant to a
// particular identifier during type-checking, but that are not
struct Qualifiers {
  Qualifiers() = default;

  // Qualifiers::Constant() is a qualifier indicating that the value of an
  // expression is known at compile-time.
  static constexpr Qualifiers Constant() { return Qualifiers(0b0001); }

  // Qualifiers::Reference() is a qualifier indicating the corresponding
  // expressions is guaranteed to have an address in memory.
  static constexpr Qualifiers Reference() { return Qualifiers(0b0010); }

  // Qualifiers::Buffer() is a qualifier indicating the corresponding
  // expressions has an address in memory that is part of a (possibly larger)
  // buffer, making it meaningful to do arithmetic on the the address.
  static constexpr Qualifiers Buffer() { return Qualifiers(0b0110); }

  // Qualifiers::Error() is a qualifier indicating the an error was found during
  // type-checking of the corresponding expression. There was sufficient
  // information that we can reasonably proceed with the expression's computed
  // type, but that any compile-time evaluation depending on this expression
  // will not be possible due to the error.
  static constexpr Qualifiers Error() { return Qualifiers(0b1000); }

  friend constexpr bool operator==(Qualifiers lhs, Qualifiers rhs) {
    return lhs.flags_ == rhs.flags_;
  }
  friend constexpr bool operator!=(Qualifiers lhs, Qualifiers rhs) {
    return not(lhs == rhs);
  }
  friend constexpr bool operator<=(Qualifiers lhs, Qualifiers rhs) {
    return (lhs.flags_ | rhs.flags_) == rhs.flags_;
  }
  friend constexpr bool operator>=(Qualifiers lhs, Qualifiers rhs) {
    return rhs <= lhs;
  }
  friend constexpr bool operator<(Qualifiers lhs, Qualifiers rhs) {
    return lhs != rhs and lhs <= rhs;
  }
  friend constexpr bool operator>(Qualifiers lhs, Qualifiers rhs) {
    return rhs < lhs;
  }
  constexpr Qualifiers& operator&=(Qualifiers rhs) {
    flags_ &= rhs.flags_;
    return *this;
  }
  constexpr Qualifiers& operator|=(Qualifiers rhs) {
    flags_ |= rhs.flags_;
    return *this;
  }
  friend constexpr Qualifiers operator&(Qualifiers lhs, Qualifiers rhs) {
    return lhs &= rhs;
  }
  friend constexpr Qualifiers operator|(Qualifiers lhs, Qualifiers rhs) {
    return lhs |= rhs;
  }
  constexpr Qualifiers operator~() const {
    return Qualifiers(uint8_t{0b1111} ^ flags_);
  }

  template <typename H>
  friend H AbslHashValue(H h, Qualifiers q) {
    return H::combine(std::move(h), q.flags_);
  }

 private:
  explicit constexpr Qualifiers(uint8_t flags) : flags_(flags) {}

  uint8_t flags_ = 0;
};

using QualifiedType = core::QualifiedType<Qualifiers>;

// Returns a `QualifiedType` that consists of `t` along with the "constant"
// qualifier.
QualifiedType Constant(core::Type t);

// Returns a `QualifiedType` that has the same underlying type as `t` and the
// same qualifiers as `t` with the addition of the "constant" qualifier.
QualifiedType Constant(QualifiedType t);

// Returns a `QualifiedType` that represents an error.
QualifiedType Error();

// Returns a `QualifiedType` that has the same underlying type as `t` and the
// same qualifiers as `t` with the addition of the "error" qualifier.
QualifiedType Error(QualifiedType t);

enum class Primitive : uint8_t {
  Bool,
  Char,
  Byte,
  F32,
  F64,
  Type,
  Integer,
  Module,
  Error
};
using PrimitiveType = core::FiniteSetType<Primitive>;

// In general, arithmetic is not allowed on pointers in Icarus. However, a more
// specialized "buffer pointer" type does allow for arithmetic. Specifically,
// the type category `BufferPointerType` represents the affine space of the
// system's address space. Integers may be added or subtracted to buffer
// pointers to produce new buffer pointers, and buffer pointers may be
// subtracted from each other to produce signed integer values representing the
// distance between those values.
struct BufferPointerType : core::TypeCategory<BufferPointerType, core::Type> {
  explicit BufferPointerType(
      core::TypeSystemSupporting<BufferPointerType> auto& s, core::Type t)
      : core::TypeCategory<BufferPointerType, core::Type>(s, t) {}

  core::Type pointee() const { return std::get<0>(decompose()); }
};

// The `SliceType` category represents a non-owning view of a range of values of
// a given type stored in contiguous memory (which we call a "slice").
struct SliceType : core::TypeCategory<SliceType, core::Type> {
  explicit SliceType(core::TypeSystemSupporting<SliceType> auto& s,
                     core::Type t)
      : core::TypeCategory<SliceType, core::Type>(s, t) {}

  core::Type pointee() const { return std::get<0>(decompose()); }
};

using TypeSystem =
    core::TypeSystem<PrimitiveType, core::SizedIntegerType, core::ParameterType,
                     core::PointerType, BufferPointerType, SliceType,
                     core::FunctionType>;

inline constexpr core::Type Bool =
    PrimitiveType(base::meta<TypeSystem>, Primitive::Bool);
inline constexpr core::Type Char =
    PrimitiveType(base::meta<TypeSystem>, Primitive::Char);
inline constexpr core::Type Byte =
    PrimitiveType(base::meta<TypeSystem>, Primitive::Byte);
inline constexpr core::Type F32 =
    PrimitiveType(base::meta<TypeSystem>, Primitive::F32);
inline constexpr core::Type F64 =
    PrimitiveType(base::meta<TypeSystem>, Primitive::F64);
inline constexpr core::Type Type =
    PrimitiveType(base::meta<TypeSystem>, Primitive::Type);
inline constexpr core::Type Integer =
    PrimitiveType(base::meta<TypeSystem>, Primitive::Integer);
inline constexpr core::Type Module =
    PrimitiveType(base::meta<TypeSystem>, Primitive::Module);
inline constexpr core::Type ErrorType =
    PrimitiveType(base::meta<TypeSystem>, Primitive::Error);

inline core::Type I(uint32_t bits) {
  return core::SizedIntegerType::I<TypeSystem>(bits);
}
inline core::Type U(uint32_t bits) {
  return core::SizedIntegerType::U<TypeSystem>(bits);
}

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TYPE_SYSTEM_H
