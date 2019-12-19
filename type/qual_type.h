#ifndef ICARUS_TYPE_QUAL_TYPE_H
#define ICARUS_TYPE_QUAL_TYPE_H

#include <iosfwd>
#include "type/type.h"

namespace type {

// `QualType` represents a type along with extra data determining whether or not
// an expression of that qualified type is constant in the given context. This
// is slightly different than being known at compile-time, though it implies
// being known at compile-time. Because this is statically assigned to each
// expression during compilation, this has more to do with const-ness in the
// current context, regardless of whether it is being executed at compile-time
// or run-time.
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
// `QualType` also has a builtin error value (the default constructed value).
struct QualType {
  explicit constexpr QualType() : QualType(nullptr, false) {}

  explicit constexpr QualType(Type const *t, bool b)
      : data_(reinterpret_cast<uintptr_t>(t) | b) {}

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
    return os << absl::StrCat(q.constant() ? "const[" : "non-const[",
                              q.type()->to_string(), "]");
  }

 private:
  uintptr_t data_ = 0;
};

}  // namespace type

#endif  // ICARUS_TYPE_QUAL_TYPE_H
