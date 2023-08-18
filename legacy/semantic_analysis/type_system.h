#ifndef ICARUS_SEMANTIC_ANALYSIS_TYPE_SYSTEM_H
#define ICARUS_SEMANTIC_ANALYSIS_TYPE_SYSTEM_H

#include <ostream>

#include "absl/numeric/int128.h"
#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "core/alignment.h"
#include "core/bytes.h"
#include "core/integer.h"
#include "core/type_contour.h"
#include "core/type_system/finite_set.h"
#include "core/type_system/function.h"
#include "core/type_system/parameter.h"
#include "core/type_system/pointer.h"
#include "core/type_system/sized_integer.h"
#include "core/type_system/type_system.h"
#include "data_types/char.h"
#include "module/unique_id.h"
#include "serialization/type_system.pb.h"

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

  // Qualifiers::Temporary() is a qualifier indicating the corresponding
  // expressions has no guaranteed location in memory.
  static constexpr Qualifiers Temporary() { return ~Buffer(); }

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

// Returns a `QualifiedType` that consists of `t` along with the "reference"
// qualifier.
QualifiedType Reference(core::Type t);

// Returns a `QualifiedType` that has the same underlying type as `t` and the
// same qualifiers as `t` with the addition of the "reference" qualifier.
QualifiedType Reference(QualifiedType t);

// Returns a `QualifiedType` that represents an error.
QualifiedType Error();

// Returns a `QualifiedType` that has the same underlying type as `t` and the
// same qualifiers as `t` with the addition of the "error" qualifier.
QualifiedType Error(QualifiedType t);

// Returns a `QualifiedType` that has the underlying type `ErrorType` (defined
// below) and has the qualifiers `q & Qualifiers::Error()`.
QualifiedType Error(Qualifiers q);

// Returns a `QualifiedType` whose underlying type is `t` and whose qualifiers
// as just the "error" qualifier.
QualifiedType Error(core::Type t);

inline constexpr core::Type Bool =
    PrimitiveType(nth::type<TypeSystem>, Primitive::Bool);
inline constexpr core::Type Char =
    PrimitiveType(nth::type<TypeSystem>, Primitive::Char);
inline constexpr core::Type Byte =
    PrimitiveType(nth::type<TypeSystem>, Primitive::Byte);
inline constexpr core::Type F32 =
    PrimitiveType(nth::type<TypeSystem>, Primitive::F32);
inline constexpr core::Type F64 =
    PrimitiveType(nth::type<TypeSystem>, Primitive::F64);
inline constexpr core::Type Type =
    PrimitiveType(nth::type<TypeSystem>, Primitive::Type);
inline constexpr core::Type Integer =
    PrimitiveType(nth::type<TypeSystem>, Primitive::Integer);
inline constexpr core::Type Module =
    PrimitiveType(nth::type<TypeSystem>, Primitive::Module);
inline constexpr core::Type NoReturn=
    PrimitiveType(nth::type<TypeSystem>, Primitive::NoReturn);
inline constexpr core::Type EmptyArray =
    PrimitiveType(nth::type<TypeSystem>, Primitive::EmptyArray);
inline constexpr core::Type NullPtr =
    PrimitiveType(nth::type<TypeSystem>, Primitive::NullPtr);
inline constexpr core::Type ErrorType =
    PrimitiveType(nth::type<TypeSystem>, Primitive::Error);

inline core::Type I(uint32_t bits) {
  return core::SizedIntegerType::I<TypeSystem>(bits);
}
inline core::Type U(uint32_t bits) {
  return core::SizedIntegerType::U<TypeSystem>(bits);
}

bool IsIntegral(core::Type t);
bool IsNumeric(core::Type t);

std::string DebugQualifiedType(QualifiedType qt);
                             
std::string DebugType(core::Type t);

template <typename F>
bool WithPrimitiveType(core::Type t, F&& f) {
  using fn_type = std::decay_t<F>;
  // TODO: Support Integer, Module, and maybe error.
  if constexpr (requires { (void)&fn_type::template operator()<bool>; }) {
    if (t == Bool) {
      std::forward<F>(f).template operator()<bool>();
      return true;
    }
  }
  if constexpr (requires {
                  (void)&fn_type::template operator()<data_types::Char>;
                }) {
    if (t == Char) {
      std::forward<F>(f).template operator()<data_types::Char>();
      return true;
    }
  }
  if constexpr (requires { (void)&fn_type::template operator()<std::byte>; }) {
    if (t == Byte) {
      std::forward<F>(f).template operator()<std::byte>();
      return true;
    }
  }
  if constexpr (requires { (void)&fn_type::template operator()<core::Type>; }) {
    if (t == Type) {
      std::forward<F>(f).template operator()<core::Type>();
      return true;
    }
  }
  if constexpr (requires { (void)&fn_type::template operator()<int8_t>; }) {
    if (t == I(8)) {
      std::forward<F>(f).template operator()<int8_t>();
      return true;
    }
  }
  if constexpr (requires { (void)&fn_type::template operator()<int16_t>; }) {
    if (t == I(16)) {
      std::forward<F>(f).template operator()<int16_t>();
      return true;
    }
  }
  if constexpr (requires { (void)&fn_type::template operator()<int32_t>; }) {
    if (t == I(32)) {
      std::forward<F>(f).template operator()<int32_t>();
      return true;
    }
  }
  if constexpr (requires { (void)&fn_type::template operator()<int64_t>; }) {
    if (t == I(64)) {
      std::forward<F>(f).template operator()<int64_t>();
      return true;
    }
  }
  if constexpr (requires { (void)&fn_type::template operator()<uint8_t>; }) {
    if (t == U(8)) {
      std::forward<F>(f).template operator()<uint8_t>();
      return true;
    }
  }
  if constexpr (requires { (void)&fn_type::template operator()<uint16_t>; }) {
    if (t == U(16)) {
      std::forward<F>(f).template operator()<uint16_t>();
      return true;
    }
  }
  if constexpr (requires { (void)&fn_type::template operator()<uint32_t>; }) {
    if (t == U(32)) {
      std::forward<F>(f).template operator()<uint32_t>();
      return true;
    }
  }
  if constexpr (requires { (void)&fn_type::template operator()<uint64_t>; }) {
    if (t == U(64)) {
      std::forward<F>(f).template operator()<uint64_t>();
      return true;
    }
  }
  if constexpr (requires { (void)&fn_type::template operator()<float>; }) {
    if (t == F32) {
      std::forward<F>(f).template operator()<float>();
      return true;
    }
  }
  if constexpr (requires { (void)&fn_type::template operator()<double>; }) {
    if (t == F64) {
      std::forward<F>(f).template operator()<double>();
      return true;
    }
  }
  return false;
}

bool PassInRegister(QualifiedType type);
bool PassInRegister(core::TypeContour contour);
core::Bytes SizeOf(core::Type type);
core::Alignment AlignmentOf(core::Type type);
core::TypeContour ContourOf(core::Type type);

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TYPE_SYSTEM_H
