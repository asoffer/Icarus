#ifndef ICARUS_TYPE_QUAL_TYPE_H
#define ICARUS_TYPE_QUAL_TYPE_H

#include <array>
#include <iosfwd>
#include <type_traits>
#include <vector>

#include "absl/types/span.h"
#include "type/type.h"

namespace type {

// As part of type verification, every expression in the syntax tree is
// annotated with a `QualType`. `QualType` represents the type of an expression
// (i.e., the one annotated with the `QualType`) along with extra data which is
// not considered to be part of the type, but still dictates what operations can
// be applied to the expression. This data includes:
//
// * Constness: Whether or not the value of the expression is known at
//   compile-time
// * Value category: Whether this expression can have it's address taken, and
//   whether this element is one in a contiguous collection.
//
// `QualType` also has a builtin error value (the default constructed value).
//
// ## Qualifiers
//
// Qualifiers indicate properties of an expression that are not included in the
// expression's type but do affect type-verification. This may seem
// counter-intuitive if you have never encountered it before. A common example
// would be that, while in general, applying the `&` operator to an expression
// produces a pointer, not every expression is allowed to have the operator
// applied. Specifically, only expressions that represent a storage location can
// have their address taken. Given a boolean variable, `b`, the expression `&b`
// is valid, but `&true` would not be valid, even though `b` and `true` have the
// same type (namely, `bool`).
//
// Whether an expression can have its address taken is just one example of such
// a type qualifier.
//
//
// ### Value category
//
// An expression's qualifiers corresponding to value category take on one of
// three kinds:
// * Buffer    -- The expression represents a storage location that is part of a
//                larger buffer. Not only can the address of the expression be
//                computed, but arithmetic may be done on its pointer type.
// * Storage   -- The expression represents a storage location, but the location
//                is not to be part of a buffer.
// * None      -- The expression does not represent a storage location.
//
//
// ### Constness
//
// An expression's qualifiers corresponding to constness take on one of three
// kinds:
// * Constant           -- The value is known at compile-time and not dependent
//                         on any constant parameters. It can be entirely
//                         evaluated it its context.
// * DependentConstant  -- The value is a compile-time constant, but that
//                         constant is dependent on the value of compile-time
//                         constant parameters of its context. It cannot be
//                         computed without knowledge of those values.
// * Runtime            -- The value is not a compile-time constant.
struct Qualifiers {
  static constexpr Qualifiers Unqualified() { return Qualifiers(0b0000); }
  static constexpr Qualifiers DependentConstant() { return Qualifiers(0b0001); }
  static constexpr Qualifiers Constant() { return Qualifiers(0b0011); }
  static constexpr Qualifiers Storage() { return Qualifiers(0b0100); }
  static constexpr Qualifiers Buffer() { return Qualifiers(0b1100); }
  static constexpr Qualifiers All() { return Qualifiers(0b1111); }

  friend constexpr Qualifiers operator|(Qualifiers lhs, Qualifiers rhs) {
    return Qualifiers(lhs.val_ | rhs.val_);
  }
  friend constexpr Qualifiers operator&(Qualifiers lhs, Qualifiers rhs) {
    return Qualifiers(lhs.val_ & rhs.val_);
  }
  constexpr Qualifiers &operator|=(Qualifiers rhs) {
    val_ |= rhs.val_;
    return *this;
  }
  constexpr Qualifiers &operator&=(Qualifiers rhs) {
    val_ &= rhs.val_;
    return *this;
  }

  friend constexpr Qualifiers operator~(Qualifiers q) {
    return Qualifiers(All().val_ - q.val_);
  }

  friend constexpr bool operator==(Qualifiers lhs, Qualifiers rhs) {
    return lhs.val_ == rhs.val_;
  }
  friend constexpr bool operator!=(Qualifiers lhs, Qualifiers rhs) {
    return not(lhs == rhs);
  }
  friend bool operator<(Qualifiers lhs, Qualifiers rhs) {
    return (lhs | rhs) == lhs and lhs != rhs;
  }
  friend bool operator>(Qualifiers lhs, Qualifiers rhs) { return rhs < lhs; }
  friend bool operator<=(Qualifiers lhs, Qualifiers rhs) {
    return (lhs | rhs) == rhs;
  }
  friend bool operator>=(Qualifiers lhs, Qualifiers rhs) { return rhs <= lhs; }

  template <typename H>
  friend H AbslHashValue(H h, Qualifiers const &q) {
    return H::combine(std::move(h), q.val_);
  }

  static constexpr Qualifiers FromValue(uint8_t value) {
    return Qualifiers(value);
  }
  uint8_t value() const { return val_; }

 private:
  friend struct QualType;
  constexpr explicit Qualifiers(uint8_t val) : val_(val) {}
  uint8_t val_;
};

inline std::ostream &operator<<(std::ostream &os, Qualifiers quals) {
  static constexpr std::array kPrintData{
      std::pair{Qualifiers::Constant(), "const"},
      std::pair{Qualifiers::DependentConstant(), "dependent-const"},
      std::pair{Qualifiers::Buffer(), "buffer"},
      std::pair{Qualifiers::Storage(), "storage"}};
  char const *sep = "";
  os << "[";
  for (auto [q, s] : kPrintData) {
    if (quals >= q) {
      quals = (quals & ~q);
      os << std::exchange(sep, ", ") << s;
    }
  }
  return os << "]";
}

struct QualType {
  static absl::Span<type::QualType const> ErrorSpan();

  explicit QualType() : QualType(Type(), Qualifiers::Unqualified()) {}
  explicit QualType(Type t, Qualifiers quals)
      : type_(t), quals_(quals), error_(false) {}

  static QualType Error() { return QualType(); }

  static QualType Constant(Type t) {
    return QualType(t, Qualifiers::Constant());
  }

  static QualType NonConstant(Type t) {
    return QualType(t, Qualifiers::Unqualified());
  }

  Type type() const { return type_; }

  // Sets an indicator on this QualType indicating that the expression with this
  // `QualType` is indeed known to have this type but that the was an
  // irrecoverable error found while processing it. This allows type-checking to
  // proceed but has later stages of code emission (either for runtime or
  // compile-time evaluation) to recognize a failure. A simple example would be:
  //
  // ```
  // Suit ::= enum { Clubs \\ Diamonds \\ Hearts \\ Spades }
  //
  // my_suit := Suit.Club // Error
  // ```
  //
  // It is clear that `my_suit` has type `Suit`, but the enumerator chosen is
  // mispelled so while it is safe to proceed with type-checking,
  // code-generation must not happen.
  //
  constexpr void MarkError() { error_ = true; }
  constexpr bool HasErrorMark() const { return error_ or not ok(); }

  constexpr Qualifiers quals() const { return quals_; }
  constexpr void set_quals(Qualifiers q) { quals_ = q; }

  constexpr void remove_constant() { quals_ &= ~Qualifiers::Constant(); }
  constexpr bool constant() const {
    return (quals_ & Qualifiers::Constant()).val_;
  }

  bool ok() const { return type() != Type(); }
  explicit operator bool() const { return ok(); }

  constexpr QualType const &operator*() const { return *this; }

  friend bool operator==(QualType lhs, QualType rhs) {
    return lhs.type_ == rhs.type_ and lhs.quals_ == rhs.quals_;
  }

  friend bool operator!=(QualType lhs, QualType rhs) { return !(lhs == rhs); }

  template <typename H>
  friend H AbslHashValue(H h, QualType q) {
    // Even when these are holding pointers to expanded data, it's okay to hash
    // because we deduplicate them on insertion.
    return H::combine(std::move(h), q.type_, q.quals_, q.error_);
  }

  friend std::ostream &operator<<(std::ostream &os, QualType q);

  std::string to_string() const {
    std::stringstream ss;
    ss << *this;
    return ss.str();
  }

 private:
  Type type_;
  Qualifiers quals_;
  bool error_;
};

}  // namespace type

#endif  // ICARUS_TYPE_QUAL_TYPE_H
