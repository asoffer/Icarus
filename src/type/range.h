#ifndef ICARUS_TYPE_RANGE_H
#define ICARUS_TYPE_RANGE_H

#include "type.h"

namespace type {
struct Range : public Type {
  TYPE_FNS(Range);

  Range(const Type *t) : end_type(t) {}

  const Type *end_type;
};

const Range *Rng(const Type *t);
} // namespace type
#endif // ICARUS_TYPE_RANGE_H
