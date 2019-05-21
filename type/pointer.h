#ifndef ICARUS_TYPE_POINTER_H
#define ICARUS_TYPE_POINTER_H

#include "type.h"

namespace type {
struct Pointer : public Type {
  TYPE_FNS(Pointer);
  Pointer(Type const *t) : pointee(t) {}

#include "visitor/type_visitors.xmacro.h"

  Type const *pointee;
};

// Like Pointer but allows indexing and pointer arithmetic.
struct BufferPointer : public Pointer {
  BufferPointer() = delete;

#include "visitor/type_visitors.xmacro.h"

  void WriteTo(std::string *result) const override;
  BufferPointer(Type const *t) : Pointer(t) {}
};

Pointer const *Ptr(Type const *t);
BufferPointer const *BufPtr(Type const *t);
}  // namespace type
#endif  // ICARUS_TYPE_POINTER_H
