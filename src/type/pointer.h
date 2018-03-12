#ifndef ICARUS_TYPE_POINTER_H
#define ICARUS_TYPE_POINTER_H

#include "type.h"

namespace type {
struct Pointer : public Type {
  TYPE_FNS(Pointer);
  Pointer(const Type *t) : pointee(t) {}
  const Type *pointee;
};

const Pointer *Ptr(const Type *t);
} // namespace type
#endif // ICARUS_TYPE_POINTER_H
