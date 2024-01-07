#ifndef ICARUS_TYPE_BASIC_H
#define ICARUS_TYPE_BASIC_H

#include <array>
#include <concepts>
#include <span>
#include <utility>

#include "jasmin/core/value.h"
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

// Objects of type `Type` represent types within the modeled type-system. The
// type `Type` is regular (i.e., can be safely copied, compared for equality,
// etc as one would expect `int` to be). Two `Type`s are considered equal if and
// only if they represent the same type in the type-system. A `Type` is
// precisely 64-bits wide, and its most-significant 8 bits must always be unset.
struct Type {
  Type() = default;

  enum class Kind : uint8_t {
#define IC_XMACRO_TYPE_KIND(kind) kind,
#include "common/language/type_kind.xmacro.h"
  };

  friend void NthPrint(auto& p, auto&, Kind k) {
    static constexpr std::array Names = {
#define IC_XMACRO_TYPE_KIND(kind) #kind,
#include "common/language/type_kind.xmacro.h"
    };
    p.write("Type::Kind::");
    p.write(Names[static_cast<std::underlying_type_t<Kind>>(k)]);
  }

  constexpr Kind kind() const {
    return static_cast<Kind>((data_ >> 48) & 0xff);
  }

  uint64_t index() const { return data_ & uint64_t{0x0000'ffff'ffff'ffff}; }

  friend bool operator==(Type, Type) = default;

  template <typename H>
  friend H AbslHashValue(H h, Type t) {
    return H::combine(std::move(h), t.data_);
  }

  friend bool IcarusDeserializeValue(std::span<jasmin::Value const> values,
                                     Type& t) {
    if (values.size() != 1) { return false; }
    t.data_ = values.front().raw_value();
    return true;
  }

  // Defines implicit constructors from each specific type-kind, as well as
  // functions which convert to specific types. These functions are all named by
  // the specific type-kind prefixed with "As", so that `AsSpecificType` would
  // convert to `SpecificType`.
#define IC_XMACRO_TYPE_KIND(kind)                                              \
  Type(kind##Type t);                                                          \
  kind##Type As##kind() const;
#include "common/language/type_kind.xmacro.h"

  friend void NthPrint(auto& p, auto& f, Type t);

 private:
  friend struct QualifiedType;

  explicit constexpr Type(uint64_t data) : data_(data) {}

  uint64_t data_;
};

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
  explicit constexpr Qualifier(uint8_t data) : data_(data) {}

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
      : data_(static_cast<uint64_t>(q.data_) << 56 | t.data_) {}

  friend bool operator==(QualifiedType, QualifiedType) = default;

  bool constant() const { return qualifier() >= Qualifier::Constant(); }
  bool addressable() const { return qualifier() >= Qualifier::Addressable(); }

  template <typename H>
  friend H AbslHashValue(H h, QualifiedType q) {
    return H::combine(std::move(h), q.data_);
  }

  friend void NthPrint(auto& p, auto& f, QualifiedType qt) {
    f(p, qt.qualifier());
    p.write(".(");
    f(p, qt.type());
    p.write(")");
  }

  constexpr Qualifier qualifier() const { return Qualifier(data_ >> 56); }
  constexpr Type type() const {
    return Type(data_ & uint64_t{0x00ffffff'ffffffff});
  }

 private:
  uint64_t data_;
};

// Returns the number of `jasmin::Value`s required to hold a value of the given
// type `t`.
size_t JasminSize(Type t);

TypeContour Contour(Type t);

namespace internal_type {

struct BasicType {
  explicit BasicType() = default;
  explicit constexpr BasicType(Type::Kind k, uint64_t n)
      : data_((static_cast<uint64_t>(k) << 48) | n) {}

  friend bool operator==(BasicType, BasicType) = default;
  friend bool operator!=(BasicType, BasicType) = default;

  template <typename H>
  friend H AbslHashValue(H h, std::derived_from<BasicType> auto t) {
    return H::combine(std::move(h), t.data_);
  }

 protected:
  uint64_t data() const { return data_ & uint64_t{0x0000ffff'ffffffff}; }

 private:
  friend Type;
  uint64_t data_;
};

}  // namespace internal_type
}  // namespace ic::type

#endif  // ICARUS_TYPE_BASIC_H
