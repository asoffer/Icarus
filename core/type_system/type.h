#ifndef ICARUS_CORE_TYPE_SYSTEM_TYPE_H
#define ICARUS_CORE_TYPE_SYSTEM_TYPE_H

#include <climits>
#include <optional>
#include <concepts>
#include <cstdint>
#include <cstring>

#include "base/meta.h"

namespace core {
namespace internal_type {

constexpr size_t QualifierBits = 8;

}  // namespace internal_type

// Each kind of type (e.g., sized-integer, pointer, etc) must use the CRTP base
// `TypeCategory` declared here and defined in "core/type_system.h"
template <typename , typename... >
struct TypeCategory;

// A `TypeSystem` (declared here and defined in "core/type_system.h") represents
// a collection of `TypeCategory`s along with storage for the types constructed
// from that type system.
template <typename...>
struct TypeSystem;

// A concept matching `TypeSystem` instantiations which support the type
// category `T`.
template <typename TS, typename T>
concept TypeSystemSupporting =
    (base::meta<TS>.template is_a<TypeSystem>() and
     std::derived_from<TS, typename T::manager_type>);

// A concept matching any type that can be used as qualifiers for a
// `QualifiedType` instantiation.
template <typename Q>
concept TypeQualifiers = std::is_trivially_copyable_v<Q> and std::regular<Q> and
                         sizeof(Q) <= (internal_type::QualifierBits / CHAR_BIT);

// A `Type` represents a value stored in a `TypeSystem`. Two types can be
// compared for equality or have a specific `TypeCategory` extracted from them.
//
// Note that each type belongs to only a single `TypeSystem` (the type system
// used to create it). Behavior is undefined if any other `TypeSystem` is used
// to access this type (via any member function, including `is`, `get`, or
// `get_if`).
struct Type {
  // The number of bits reserved inside this type for representing type
  // categories.
  static constexpr size_t CategoryBits = 8;
  // The number of bits reserved to hold `Qualifiers`. While `Type` itself is
  // not responsible for holding any `Qualifiers`, we ensure that
  // `QualifiedType` is the same size as `Type` to ensure they are both cheap to
  // copy. Because `QualifiedType` stores qualifiers, we must guarantee this
  // space is available as well.
  static constexpr size_t QualifierBits = internal_type::QualifierBits;

  // Returns an integer representing the index of the type category within the
  // `TypeSystem` that created this `Type`.
  constexpr uint64_t category() const {
    return (representation_ >> QualifierBits) &
           ((uint64_t{1} << CategoryBits) - 1);
  }

  constexpr uint64_t index() const {
    return representation_ >> (CategoryBits + QualifierBits);
  }

  // Returns `true` if and only if `*this` type was created from the type
  // category `Cat`.
  template <typename Category>
  bool is(TypeSystemSupporting<Category> auto& sys) const {
    return category() == sys.template index<Category>();
  }

  // Returns a value of type `Category` from which `*this` was created. Behavior
  // is undefined if `*this` was not created from `Category`. This return value
  // of function is guaranteed to satisfy the constraint that:
  // `t == static_cast<Type>(t.get<Category>(sys))`
  template <typename Category>
  Category get(TypeSystemSupporting<Category> auto& sys) const {
    ASSERT(this->is<Category>(sys) == true);
    return Category::Construct(*this, sys);
  }

  // Returns a value of type `Category` from which `*this` was created if the
  // type was created from `Category`, and `std::nullopt` otherwise.
  template <typename Category>
  std::optional<Category> get_if(
      TypeSystemSupporting<Category> auto& sys) const {
    if (not is<Category>(sys)) { return std::nullopt; }
    return get<Category>(sys);
  }

  template <typename H>
  friend H AbslHashValue(H h, Type t) {
    return H::combine(std::move(h), t.representation());
  }

  friend constexpr bool operator==(Type lhs, Type rhs) {
    return lhs.representation() == rhs.representation();
  }
  friend constexpr bool operator!=(Type lhs, Type rhs) {
    return not(lhs == rhs);
  }

 private:
  template <typename, typename...>
  friend struct TypeCategory;

  template <TypeQualifiers>
  friend struct QualifiedType;

  constexpr uint64_t representation() const { return representation_; }

  constexpr void set_category(uint8_t v) {
    constexpr uint64_t mask =
        ~(((uint64_t{1} << CategoryBits) - uint64_t{1}) << QualifierBits);
    representation_ &= mask;
    representation_ += v << QualifierBits;
  }

  constexpr void set_value(uint64_t v) {
    representation_ &= static_cast<uint64_t>(
        (uint64_t{1} << (CategoryBits + QualifierBits)) - uint64_t{1});
    representation_ += v << (CategoryBits + QualifierBits);
  }

  // The least-significant `QualifierBits` bits are always zero. This enables
  // QualifiedType to store qualifiers in these bits without using extra space.
  // The next-least significant `CategoryBits` represent the type category. The
  // remaining bits represent the value of the type with respect to that
  // category.
  uint64_t representation_ = 0;
};
static_assert(sizeof(Type) == sizeof(uint64_t));

// A `QualifiedType<Q>` represents a type along with qualifiers `Q`. Qualifiers
// represent properties that are important to track during type-checking, but
// are not to be considered part of the type itself.
template <TypeQualifiers Q>
struct QualifiedType {
  explicit QualifiedType() = default;
  explicit QualifiedType(Type t) : QualifiedType(t, Q()) {}

  // Constructs a `QualifiedType` from its constituent type and qualifiers.
  explicit QualifiedType(Type t, Q q) {
    uint8_t result;
    std::memcpy(&result, &q, sizeof(Q));
    representation_ = t.representation() | static_cast<uint64_t>(result);
  }

  // Returns the type underlying `Type`.
  Type type() const {
    Type t;
    uint64_t masked_type =
        representation_ &
        ~static_cast<uint64_t>((uint64_t{1} << Type::QualifierBits) - 1);
    std::memcpy(&t, &masked_type, sizeof(t));
    return t;
  }

  // Returns the type underlying type qualifiers.
  Q qualifiers() const {
    static constexpr uint64_t kQualifierMask =
        (uint64_t{1} << Type::QualifierBits) - 1;
    Q q;
    uint8_t low_byte = representation_ & kQualifierMask;
    std::memcpy(&q, &low_byte, sizeof(q));
    return q;
  }

  template <typename H>
  friend H AbslHashValue(H h, QualifiedType qt) {
    return H::combine(std::move(h), qt.type(), qt.qualifiers());
  }

  friend bool operator==(QualifiedType lhs, QualifiedType rhs) {
    return lhs.type() == rhs.type() and lhs.qualifiers() == rhs.qualifiers();
  }

  friend bool operator!=(QualifiedType lhs, QualifiedType rhs) {
    return not(lhs == rhs);
  }

 private:
  uint64_t representation_;
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_TYPE_H
