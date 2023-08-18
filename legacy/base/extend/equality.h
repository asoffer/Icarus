#ifndef ICARUS_BASE_EXTEND_EQUALITY_H
#define ICARUS_BASE_EXTEND_EQUALITY_H

#include "base/extend.h"

namespace base {

template <typename T>
struct EqualityExtension {
  friend bool operator==(T const &lhs, T const &rhs) {
    return lhs.field_refs() == rhs.field_refs();
  }

  friend bool operator!=(T const &lhs, T const &rhs) { return not(lhs == rhs); }
};

}  // namespace base

#endif  // ICARUS_BASE_EXTEND_EQUALITY_H
