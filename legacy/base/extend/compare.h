#ifndef ICARUS_BASE_EXTEND_COMPARE_H
#define ICARUS_BASE_EXTEND_COMPARE_H

#include "base/extend.h"

namespace base {

template <typename T>
struct TotalOrderExtension {
  static constexpr auto dependencies = nth::type_sequence<EqualityExtension<T>>;

  friend bool operator<(T const &lhs, T const &rhs) {
    return lhs.field_refs() < rhs.field_refs();
  }

  friend bool operator>(T const &lhs, T const &rhs) { return rhs < lhs; }
  friend bool operator<=(T const &lhs, T const &rhs) { return not(rhs < lhs); }
  friend bool operator>=(T const &lhs, T const &rhs) { return not(rhs < lhs); }
};

}  // namespace base

#endif  // ICARUS_BASE_EXTEND_COMPARE_H
