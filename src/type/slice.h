#ifndef ICARUS_TYPE_SLICE_H
#define ICARUS_TYPE_SLICE_H

#include "type.h"

namespace type {
struct Array;

struct Slice : public Type {
  TYPE_FNS(Slice);

  Slice(const Array *a) : array_type(a) {}

  const Array *array_type;
};

const Slice *Slc(const Array *a);
} // namespace type
#endif // ICARUS_TYPE_SLICE_H
