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
using PrimitiveTypes = core::FiniteSetType<Primitive>;

inline constexpr core::Type Bool      = PrimitiveTypes(Primitive::Bool);
inline constexpr core::Type Char      = PrimitiveTypes(Primitive::Char);
inline constexpr core::Type Byte      = PrimitiveTypes(Primitive::Byte);
inline constexpr core::Type F32       = PrimitiveTypes(Primitive::F32);
inline constexpr core::Type F64       = PrimitiveTypes(Primitive::F64);
inline constexpr core::Type Type      = PrimitiveTypes(Primitive::Type);
inline constexpr core::Type Integer   = PrimitiveTypes(Primitive::Integer);
inline constexpr core::Type Module    = PrimitiveTypes(Primitive::Module);
inline constexpr core::Type ErrorType = PrimitiveTypes(Primitive::Error);

// Represents types whose storage is sufficient to hold values in the range
// -2^{N-1} ... 2^{N-1}-1 (inclusive) for signed types and 0 ... 2^N - 1
// (inclusive) for unsigned types.

struct SliceType : core::TypeCategory<SliceType, core::Type> {
  explicit SliceType(core::TypeSystemSupporting<SliceType> auto& s,
                     core::Type t)
      : core::TypeCategory<SliceType, core::Type>(s, t) {}

  core::Type pointee() const { return std::get<0>(decompose()); }
};

using TypeSystem = core::TypeSystem<PrimitiveTypes, core::SizedIntegerType,
                                    core::ParameterType, core::PointerType,
                                    SliceType, core::FunctionType>;

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TYPE_SYSTEM_H
