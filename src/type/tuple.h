#ifndef ICARUS_TYPE_TUPLE_H
#define ICARUS_TYPE_TUPLE_H

#include "type.h"

#include <vector>
#include <utility>

namespace type {
struct Tuple : public Type {
  TYPE_FNS(Tuple);
  Tuple(std::vector<const Type *> entries) : entries(std::move(entries)) {}

  virtual bool needs_destroy() const {
    for (const Type *t : entries) {
      if (t->needs_destroy()) { return true; }
    }
    return false;
  }

  std::vector<const Type *> entries;
};

const Type *Tup(std::vector<const Type *> types);
} // namespace type
#endif // ICARUS_TYPE_TUPLE_H
