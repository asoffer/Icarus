#ifndef ICARUS_TYPE_QUAL_TYPE_H
#define ICARUS_TYPE_QUAL_TYPE_H

#include <array>
#include <iosfwd>
#include <type_traits>
#include <vector>

#include "type/type.h"

namespace type {

// As part of type verification, every expression in the syntax tree is
// annotated with a `QualType`. `QualType` represents the type of an expression
// (i.e., the one annotated with the `QualType`) along with extra data which is
// typically not considered to be part of the type, but still dictates what
// operations can be applied to the expression. This data includes:
//
// * Constness: Whether or not the value of the expression is known at
//   compile-time
// * Expansion-size: The number of identifiers that can be bound to this
//   expression.
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
// add_two ::= (n: int64) -> int64 {
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
// *** Expansion-size ***
// Some expressions represent multiple values, as in `(1, 2, 3)`. However, it is
// not always clear syntactically that this is the case. For example, functions
// can return multiple values, so the result of a function call may have
// expansion-size greater than one. Functions that do not return a value have an
// expansion-size of zero, which is distinct from returning a unit-type.
//
// ```
// f ::= () -> (int64, bool) { return 3, true }
// (n, b) := f()
// ```
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
      os << sep << s;
      sep = ",";
    }
  }
  return os << "]";
}

namespace internal_type {

absl::Span<Type const> AddPack(absl::Span<Type const> types);

}  // namespace internal_type

struct QualType {
  explicit QualType() : QualType(nullptr, Quals::Unqualified()) {}

  explicit QualType(std::nullptr_t, Quals quals)
      : data_(static_cast<uintptr_t>(quals.val_)) {}

  // Use SFINAE  to disable braced-initialization for the type parameter. This
  // allows it to fallback to meaning the vector initializer.
  template <typename Arg,
            std::enable_if_t<std::is_convertible_v<Arg, Type>, int> = 0>
  explicit QualType(Arg t, Quals quals)
      : data_(reinterpret_cast<uintptr_t>(
                  static_cast<LegacyType const *>(Type(t).get())) |
              static_cast<uintptr_t>(quals.val_)) {}

  explicit QualType(absl::Span<Type const> ts, Quals quals) {
    num_ = ts.size();
    if (ts.size() == 1) {
      data_ = reinterpret_cast<uintptr_t>(ts[0].get()) |
              static_cast<uintptr_t>(quals.val_);
    } else {
      auto pack = internal_type::AddPack(ts);
      data_     = reinterpret_cast<uintptr_t>(pack.data()) | quals.val_;
    }
  }

  static QualType Error() { return QualType(nullptr, Quals::Unqualified()); }

  static QualType Constant(Type t) { return QualType(t, Quals::Const()); }

  static QualType NonConstant(Type t) {
    return QualType(t, Quals::Unqualified());
  }

  Type type() const {
    ASSERT(num_ == 1u);
    return reinterpret_cast<LegacyType const *>(
        data_ & ~static_cast<uintptr_t>(Quals::All().val_));
  }

  // Sets an indicator on this QualType indicating that the expression with this
  // `QualType` is indeed known to have this type but that the was an
  // irrecoverable error found while processing it. This allows type-checking to
  // proceed but has later stages of code emission (either for runtime or
  // compile-time evaluation) to recognize a failure. A simple example would be:
  //
  // ```
  //  Suit ::= enum { Clubs \\ Diamonds \\ Hearts \\ Spades }
  //
  //  my_suit := Suit.Club // Error
  //  ```
  //
  //  It is clear that `my_suit` has type `Suit`, but the enumerator chosen is
  //  mispelled so while it is safe to proceed with type-checking,
  //  code-generation must not happen.
  constexpr void MarkError() {}                          // TODO: Implement me
  constexpr bool HasErrorMark() const { return false; }  // TODO: Implement me

  constexpr Quals quals() const { return Quals(data_ & 0x7); }

  constexpr void remove_constant() {
    auto low_bits = data_ & 0x7;
    data_ &= ~uintptr_t{0x7};
    data_ |= (low_bits & (~Quals::Const()).val_);
  }
  constexpr bool constant() const { return (quals() & Quals::Const()).val_; }
  constexpr size_t expansion_size() const { return num_; }

  absl::Span<type::Type const> expanded() const {
    ASSERT(expansion_size() != 1u);
    return absl::MakeConstSpan(
        reinterpret_cast<Type const *>(
            data_ & ~static_cast<uintptr_t>(Quals::All().val_)),
        num_);
  }

  template <typename F>
  void ForEach(F f) const {
    if (expansion_size() == 1) {
      f(type());
    } else {
      for (Type t : expanded()) { f(t); }
    }
  }

  explicit constexpr operator bool() const { return data_ != 0 or num_ != 1; }
  constexpr bool ok() const { return data_ != 0 or num_ != 1; }

  constexpr QualType const &operator*() const { return *this; }

  friend bool operator==(QualType lhs, QualType rhs) {
    // Even when these are holding pointers to expanded data, it's okay to test
    // for equality because we deduplicate them on insertion.
    return lhs.data_ == rhs.data_;
  }

  friend bool operator!=(QualType lhs, QualType rhs) { return !(lhs == rhs); }

  template <typename H>
  friend H AbslHashValue(H h, QualType q) {
    // Even when these are holding pointers to expanded data, it's okay to hash
    // because we deduplicate them on insertion.
    return H::combine(std::move(h), q.data_, q.num_);
  }

  friend std::ostream &operator<<(std::ostream &os, QualType q);

  std::string to_string() const {
    std::stringstream ss;
    ss << *this;
    return ss.str();
  }

 private:
  uintptr_t data_ = 0;
  uintptr_t num_  = 1;
};

}  // namespace type

#endif  // ICARUS_TYPE_QUAL_TYPE_H
