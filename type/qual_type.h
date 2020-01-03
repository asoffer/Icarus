#ifndef ICARUS_TYPE_QUAL_TYPE_H
#define ICARUS_TYPE_QUAL_TYPE_H

#include <type_traits>
#include <iosfwd>
#include <vector>

#include "type/type.h"
#include "type/tuple.h"

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
//   (Planned, not yet implemented)
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
struct QualType {
  explicit constexpr QualType() : QualType(nullptr, false) {}

  // Use SFINAE in to disable braced-initialization for the type parameter. This
  // allows it to fallback to meaning the vector initializer.
  template <typename Arg,
            std::enable_if_t<std::is_convertible_v<Arg, Type const *>, int> = 0>
  explicit constexpr QualType(Arg t, bool b)
      : data_(reinterpret_cast<uintptr_t>(t) | b) {}

  explicit QualType(std::vector<Type const *> ts, bool b) {
    num_     = ts.size();
    auto tup = type::Tup(std::move(ts));
    data_    = reinterpret_cast<uintptr_t>(tup) | b;
  }

  static constexpr QualType Error() { return QualType(nullptr, false); }

  static constexpr QualType Constant(Type const *t) {
    return QualType(t, true);
  }

  static constexpr QualType NonConstant(Type const *t) {
    return QualType(t, false);
  }

  constexpr Type const *type() const {
    return reinterpret_cast<Type const *>(data_ & ~uintptr_t{1});
  }

  constexpr bool constant() const { return data_ & uintptr_t{1}; }
  constexpr size_t expansion_size() const { return num_; }

  explicit constexpr operator bool() const { return data_ != 0; }
  constexpr bool ok() const { return data_ != 0; }

  constexpr QualType const &operator*() const { return *this; }

  friend constexpr bool operator==(QualType lhs, QualType rhs) {
    return lhs.data_ == rhs.data_;
  }

  friend constexpr bool operator!=(QualType lhs, QualType rhs) {
    return !(lhs == rhs);
  }

  friend std::ostream &operator<<(std::ostream &os, QualType q) {
    if (not q) { return os << "error"; }
    return os << (q.constant() ? "const" : "non-const")
              << (q.expansion_size() == 1
                      ? absl::StrCat("(", q.type()->to_string(), ")")
                      : q.type()->to_string());
  }

 private:
  uintptr_t data_ = 0;
  uintptr_t num_  = 1;
};

}  // namespace type

#endif  // ICARUS_TYPE_QUAL_TYPE_H
