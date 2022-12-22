#ifndef ICARUS_BASE_EXTEND_ABSL_HASH_H
#define ICARUS_BASE_EXTEND_ABSL_HASH_H

#include "base/extend.h"
#include "base/extend/equality.h"

namespace base {

template <typename T>
struct AbslHashExtension {
  using dependencies = base::type_list<EqualityExtension<T>>;

  template <typename H>
  friend H AbslHashValue(H h, T const &t) {
    return std::apply(
        [&](auto const &...fields) {
          return H::combine(std::move(h), fields...);
        },
        t.field_refs());
  }
};

}  // namespace base

#endif  // ICARUS_BASE_EXTEND_ABSL_HASH_H
