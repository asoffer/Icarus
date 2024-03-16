#ifndef ICARUS_TYPE_BASIC_H
#define ICARUS_TYPE_BASIC_H

#include <array>
#include <concepts>
#include <span>
#include <utility>

#include "common/constants.h"
#include "jasmin/core/value.h"
#include "nth/io/deserialize/deserialize.h"
#include "nth/io/serialize/serialize.h"
#include "type/type_contour.h"

namespace ic::type {

// All types definable within the type-system can be categorized by "kind" and
// fit into one of the kinds specified in "common/language/type_kind.xmacro.h".
// Each such type must be precisely 64-bits wide, and must have the
// most-significant 8 bytes be unset in any valid representation. Furthermore,
// the second-most-significant 8 bytes must be filled with a representation of
// the corresponding `Type::Kind` defined below.
#define IC_XMACRO_TYPE_KIND(kind) struct kind##Type;
#include "common/language/type_kind.xmacro.h"

struct Qualifier {
  static constexpr Qualifier Addressable() { return Qualifier(2); }
  static constexpr Qualifier Constant() { return Qualifier(1); }
  static constexpr Qualifier Unqualified() { return Qualifier(0); }

  friend constexpr bool operator==(Qualifier, Qualifier) = default;
  friend constexpr bool operator!=(Qualifier, Qualifier) = default;
  friend constexpr bool operator<=(Qualifier lhs, Qualifier rhs) {
    return (lhs.data_ & rhs.data_) == lhs.data_;
  }
  friend constexpr bool operator>=(Qualifier lhs, Qualifier rhs) {
    return rhs <= lhs;
  }
  constexpr Qualifier& operator|=(Qualifier q) {
    data_ |= q.data_;
    return *this;
  }

  template <typename H>
  friend H AbslHashValue(H h, Qualifier q) {
    H::combine(std::move(h), q.data_);
  }

  friend void NthPrint(auto& p, auto&, Qualifier q) {
    if (q.data_ == 1) { p.write("c"); }
    if (q.data_ == 2) { p.write("a"); }
  }

 private:
  friend struct QualifiedType;
  explicit constexpr Qualifier(uint8_t data = 0) : data_(data) {}

  uint8_t data_;
};

struct QualifiedType {
  constexpr explicit QualifiedType() = default;

  static constexpr QualifiedType Unqualified(Type t) {
    return QualifiedType(Qualifier::Unqualified(), t);
  }

  static constexpr QualifiedType Constant(Type t) {
    return QualifiedType(Qualifier::Constant(), t);
  }

  constexpr explicit QualifiedType(Qualifier q, Type t)
      : qualifier_(q), type_(t) {}

  friend bool operator==(QualifiedType, QualifiedType) = default;

  bool constant() const { return qualifier() >= Qualifier::Constant(); }
  bool addressable() const { return qualifier() >= Qualifier::Addressable(); }

  template <typename H>
  friend H AbslHashValue(H h, QualifiedType q) {
    return H::combine(std::move(h), q.qualifier_, q.type_);
  }

  friend void NthPrint(auto& p, auto& f, QualifiedType qt) {
    f(p, qt.qualifier());
    p.write(".(");
    f(p, qt.type());
    p.write(")");
  }

  [[nodiscard]] constexpr Qualifier qualifier() const { return qualifier_; }
  [[nodiscard]] constexpr Type type() const { return type_; }

 private:
  Qualifier qualifier_;
  Type type_;
};

// Returns the number of `jasmin::Value`s required to hold a value of the given
// type `t`.
size_t JasminSize(Type t);

TypeContour Contour(Type t);

namespace internal_type {

struct BasicType {
  explicit BasicType() = default;
  explicit constexpr BasicType(Type::Kind k, uint32_t n)
      : data_((n << 8) | static_cast<uint8_t>(k)) {}

  friend bool operator==(BasicType, BasicType) = default;
  friend bool operator!=(BasicType, BasicType) = default;

  template <typename H>
  friend H AbslHashValue(H h, std::derived_from<BasicType> auto t) {
    return H::combine(std::move(h), t.data_);
  }

  uint32_t index() const { return data_ >> 8; }

 protected:
  uint32_t data() const { return data_ >> 8; }

 private:
  friend Type;
  uint32_t data_;
};

}  // namespace internal_type
}  // namespace ic::type

#endif  // ICARUS_TYPE_BASIC_H
