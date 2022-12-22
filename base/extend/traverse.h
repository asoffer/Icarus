#ifndef ICARUS_BASE_EXTEND_BASE_TRAVERSE_H
#define ICARUS_BASE_EXTEND_BASE_TRAVERSE_H

#include "base/extend.h"
#include "base/traverse.h"

namespace base {

template <typename T>
struct BaseTraverseExtension {
  template <typename Tr>
  friend void BaseTraverse(Tr &tr, T &t) {
    std::apply([&](auto &...fields) { base::Traverse(tr, fields...); },
               t.field_refs());
  }
};

}  // namespace base

#endif  // ICARUS_BASE_EXTEND_BASE_TRAVERSE_H
