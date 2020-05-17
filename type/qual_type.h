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
// * Value category: Whether this expression can have it's address taken.
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
  static constexpr Quals All() { return Quals(3); }

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
  friend bool operator<=(Quals lhs, Quals rhs) { return (lhs | rhs) == lhs; }
  friend bool operator>=(Quals lhs, Quals rhs) { return rhs <= lhs; }

 private:
  friend struct QualType;
  constexpr explicit Quals(uint8_t val) : val_(val) {}
  uint8_t val_;
};

inline std::ostream &operator<<(std::ostream &os, Quals quals) {
  static constexpr std::array kPrintData{std::pair{Quals::Const(), "const"},
                                         std::pair{Quals::Ref(), "ref"}};
  char const *sep = "";
  os << "[";
  for (auto [q, s] : kPrintData) {
    if ((quals & q) == q) {
      os << sep << s;
      sep = ",";
    }
  }
  return os << "]";
}

namespace internal_type {

absl::Span<Type const *const> AddPack(absl::Span<Type const *const> types);

}  // namespace internal_type

struct QualType {
  explicit constexpr QualType() : QualType(nullptr, Quals::Unqualified()) {}

  // Use SFINAE in to disable braced-initialization for the type parameter. This
  // allows it to fallback to meaning the vector initializer.
  template <typename Arg,
            std::enable_if_t<std::is_convertible_v<Arg, Type const *>, int> = 0>
  explicit constexpr QualType(Arg t, Quals quals)
      : data_(reinterpret_cast<uintptr_t>(t) |
              static_cast<uintptr_t>(quals.val_)) {}

  explicit QualType(absl::Span<Type const * const> ts, Quals quals) {
    num_ = ts.size();
    if (ts.size() == 1) {
      data_ = reinterpret_cast<uintptr_t>(ts[0]) |
              static_cast<uintptr_t>(quals.val_);
    } else {
      auto pack = internal_type::AddPack(ts);
      data_     = reinterpret_cast<uintptr_t>(pack.data()) | quals.val_;
    }
  }

  static constexpr QualType Error() {
    return QualType(nullptr, Quals::Unqualified());
  }

  static constexpr QualType Constant(Type const *t) {
    return QualType(t, Quals::Const());
  }

  static constexpr QualType NonConstant(Type const *t) {
    return QualType(t, Quals::Unqualified());
  }

  Type const *type() const {
    return reinterpret_cast<Type const *>(
        data_ & ~static_cast<uintptr_t>(Quals::All().val_));
  }

  constexpr Quals quals() const { return Quals(data_ & 0x3); }

  constexpr bool constant() const { return (quals() & Quals::Const()).val_; }
  constexpr size_t expansion_size() const { return num_; }

  absl::Span<type::Type const *const> expanded() const {
    ASSERT(expansion_size() != 1u);
    return absl::MakeConstSpan(
        reinterpret_cast<Type const *const *>(
            data_ & ~static_cast<uintptr_t>(Quals::All().val_)),
        num_);
  }

  explicit constexpr operator bool() const { return data_ != 0; }
  constexpr bool ok() const { return data_ != 0; }

  constexpr QualType const &operator*() const { return *this; }

  friend constexpr bool operator==(QualType lhs, QualType rhs) {
    return lhs.data_ == rhs.data_;
  }

  friend constexpr bool operator!=(QualType lhs, QualType rhs) {
    return !(lhs == rhs);
  }

  friend std::ostream &operator<<(std::ostream &os, QualType q);

 private:
  uintptr_t data_ = 0;
  uintptr_t num_  = 1;
};

}  // namespace type

#endif  // ICARUS_TYPE_QUAL_TYPE_H
