#ifndef ICARUS_TYPE_QUAL_TYPE_H
#define ICARUS_TYPE_QUAL_TYPE_H

#include <array>
#include <iosfwd>
#include <type_traits>
#include <vector>

#include "type/type.h"
#include "absl/types/span.h"

namespace type {

// As part of type verification, every expression in the syntax tree is
// annotated with a `QualType`. `QualType` represents the type of an expression
// (i.e., the one annotated with the `QualType`) along with extra data which is
// typically not considered to be part of the type, but still dictates what
// operations can be applied to the expression. This data includes:
//
// * Constness: Whether or not the value of the expression is known at
//   compile-time
// * Value category: Whether this expression can have it's address taken, and
//   whether this element is one in a contiguous collection.
//
// `QualType` also has a builtin error value (the default constructed value).
//
// *** Constness ***
// Whether or not an expression of that qualified type is constant in the given
// context. This is slightly different than being known at compile-time, though
// it implies being known at compile-time. Because this is statically assigned
// to each expression during compilation, this has more to do with const-ness in
// the current context, regardless of whether it is being executed at
// compile-time or run-time.
//
// For example,
//
// ```
// add_two ::= (n: i64) -> i64 {
//  TWO ::= 2
//  return n + TWO
// }
//
// FOUR ::= add_two(2)
// ```
//
// In the snippet above, `n` is qualified as non-const, because in the context
// of the function literal it is declared with `:`, despite the fact that, when
// computing `FOUR`, the constant `2` is bound to `n`.
//
struct Quals {
  static constexpr Quals Unqualified() { return Quals(0); }
  static constexpr Quals Const() { return Quals(1); }
  static constexpr Quals Ref() { return Quals(2); }
  static constexpr Quals Buf() { return Quals(6); }
  static constexpr Quals All() { return Quals(7); }

  friend constexpr Quals operator|(Quals lhs, Quals rhs) {
    return Quals(lhs.val_ | rhs.val_);
  }
  friend constexpr Quals operator&(Quals lhs, Quals rhs) {
    return Quals(lhs.val_ & rhs.val_);
  }
  constexpr Quals &operator|=(Quals rhs) {
    val_ |= rhs.val_;
    return *this;
  }
  constexpr Quals &operator&=(Quals rhs) {
    val_ &= rhs.val_;
    return *this;
  }

  friend constexpr Quals operator~(Quals q) {
    return Quals(All().val_ - q.val_);
  }

  friend constexpr bool operator==(Quals lhs, Quals rhs) {
    return lhs.val_ == rhs.val_;
  }
  friend constexpr bool operator!=(Quals lhs, Quals rhs) {
    return not(lhs == rhs);
  }
  friend bool operator<(Quals lhs, Quals rhs) {
    return (lhs | rhs) == lhs and lhs != rhs;
  }
  friend bool operator>(Quals lhs, Quals rhs) { return rhs < lhs; }
  friend bool operator<=(Quals lhs, Quals rhs) { return (lhs | rhs) == rhs; }
  friend bool operator>=(Quals lhs, Quals rhs) { return rhs <= lhs; }

  template <typename H>
  friend H AbslHashValue(H h, Quals const &q) {
    return H::combine(std::move(h), q.val_);
  }

  // private:
  friend struct QualType;
  constexpr explicit Quals(uint8_t val) : val_(val) {}
  uint8_t val_;
};

inline std::ostream &operator<<(std::ostream &os, Quals quals) {
  static constexpr std::array kPrintData{std::pair{Quals::Const(), "const"},
                                         std::pair{Quals::Buf(), "buf"},
                                         std::pair{Quals::Ref(), "ref"}};
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

  explicit QualType() : QualType(Type(), Quals::Unqualified()) {}
  explicit QualType(Type t, Quals quals)
      : type_(t), quals_(quals), error_(false) {}

  static QualType Error() { return QualType(); }

  static QualType Constant(Type t) { return QualType(t, Quals::Const()); }

  static QualType NonConstant(Type t) {
    return QualType(t, Quals::Unqualified());
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

  constexpr Quals quals() const { return quals_; }
  constexpr void set_quals(Quals q) { quals_ = q; }

  constexpr void remove_constant() { quals_ &= ~Quals::Const(); }
  constexpr bool constant() const { return (quals_ & Quals::Const()).val_; }

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
  Quals quals_;
  bool error_;
};

}  // namespace type

#endif  // ICARUS_TYPE_QUAL_TYPE_H
