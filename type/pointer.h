#ifndef ICARUS_TYPE_POINTER_H
#define ICARUS_TYPE_POINTER_H

#include "type/type.h"

namespace type {
struct Pointer : public Type {
  TYPE_FNS(Pointer);
  Pointer(Type const *t) : pointee(t) {}

#include ICARUS_TYPE_VISITOR_METHODS

  Type const *pointee;
};

// Like Pointer but allows indexing and pointer arithmetic.
struct BufferPointer : public Pointer {
  BufferPointer() = delete;

#include ICARUS_TYPE_VISITOR_METHODS

  void WriteTo(std::string *result) const override;
  BufferPointer(Type const *t) : Pointer(t) {}
};

Pointer const *Ptr(Type const *t);
BufferPointer const *BufPtr(Type const *t);
}  // namespace type
#endif  // ICARUS_TYPE_POINTER_H
