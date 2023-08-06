#ifndef ICARUS_SEMANTIC_ANALYSIS_TYPE_SYSTEM_H
#define ICARUS_SEMANTIC_ANALYSIS_TYPE_SYSTEM_H

#include <ostream>

#include "absl/numeric/int128.h"
#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "core/alignment.h"
#include "core/bytes.h"
#include "core/type_contour.h"
#include "core/type_system/finite_set.h"
#include "core/type_system/function.h"
#include "core/type_system/parameter.h"
#include "core/type_system/pointer.h"
#include "core/type_system/sized_integer.h"
#include "core/type_system/type_system.h"
#include "data_types/char.h"
#include "data_types/integer.h"
#include "module/unique_id.h"
#include "serialization/module_index.h"
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

enum class Primitive : uint8_t {
  Bool,
  Char,
  Byte,
  F32,
  F64,
  Type,
  Integer,
  Module,
  NoReturn,
  EmptyArray,
  NullPtr,
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

namespace internal_type_system {

struct ArrayTypeState
    : base::Extend<ArrayTypeState>::With<base::AbslHashExtension> {
  core::Type type;
  size_t length;
};

}  // namespace internal_type_system

// The `ArrayType` category represents a compile-time fixed-length block of
// contiguous objects all of the same type.
struct ArrayType
    : core::TypeCategory<ArrayType, internal_type_system::ArrayTypeState> {
  explicit ArrayType(core::TypeSystemSupporting<ArrayType> auto& s,
                     size_t length, core::Type t)
      : ArrayType(s, {.type = t, .length = length}) {}

  core::Type data_type() const { return std::get<0>(decompose()).type; }
  size_t length() const { return std::get<0>(decompose()).length; }

 private:
  friend TypeCategory;

  explicit ArrayType(core::TypeSystemSupporting<ArrayType> auto& s,
                     internal_type_system::ArrayTypeState state)
      : core::TypeCategory<ArrayType, internal_type_system::ArrayTypeState>(
            s, state) {}
};

// The `SliceType` category represents a non-owning view of a range of values of
// a given type stored in contiguous memory (which we call a "slice").
struct SliceType : core::TypeCategory<SliceType, core::Type> {
  explicit SliceType(core::TypeSystemSupporting<SliceType> auto& s,
                     core::Type t)
      : core::TypeCategory<SliceType, core::Type>(s, t) {}

  core::Type pointee() const { return std::get<0>(decompose()); }
};

// The `EnumType` category represents a types that have a fixed set of
// enumerated values defined by the user.
struct EnumType
    : core::TypeCategory<EnumType, module::UniqueId, size_t,
                         serialization::TypeSystem::EnumType const*> {
  explicit EnumType(core::TypeSystemSupporting<EnumType> auto& s,
                    module::UniqueId module_id, size_t index,
                    serialization::TypeSystem::EnumType const* ptr)
      : TypeCategory(s, module_id, index, ptr) {}

  auto const& enumerators() const {
    return std::get<2>(decompose())->enumerator();
  }
  bool has_member(std::string_view name) {
    return enumerators().contains(name);
  }
  std::optional<uint64_t> value(std::string_view name) {
    auto const& e = enumerators();
    auto iter     = e.find(name);
    if (iter == e.end()) { return std::nullopt; }
    return iter->second;
  }

 private:
  friend TypeCategory;
};

struct OpaqueType : core::TypeCategory<OpaqueType, size_t> {
  explicit OpaqueType(core::TypeSystemSupporting<OpaqueType> auto& s)
      : OpaqueType(s, counter.fetch_add(1, std::memory_order_relaxed)) {}

  size_t index() const { return std::get<0>(decompose()); }

  // TODO: Make private.
  explicit OpaqueType(core::TypeSystemSupporting<OpaqueType> auto& s, size_t n)
      : core::TypeCategory<OpaqueType, size_t>(s, n) {}

  static std::atomic<size_t> counter;
};

using TypeSystem =
    core::TypeSystem<PrimitiveType, core::SizedIntegerType, core::ParameterType,
                     core::PointerType, BufferPointerType, ArrayType, SliceType,
                     core::FunctionType, EnumType, OpaqueType>;

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

std::string DebugQualifiedType(QualifiedType qt, TypeSystem& ts);
std::string DebugType(core::Type t, TypeSystem& ts);

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

bool PassInRegister(QualifiedType type, TypeSystem& type_system);
bool PassInRegister(core::TypeContour contour);
core::Bytes SizeOf(core::Type type, TypeSystem& type_system);
core::Alignment AlignmentOf(core::Type type, TypeSystem& type_system);
core::TypeContour ContourOf(core::Type type, TypeSystem& type_system);

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TYPE_SYSTEM_H
