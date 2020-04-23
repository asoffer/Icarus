#ifndef ICARUS_TYPE_POINTER_H
#define ICARUS_TYPE_POINTER_H

#include "type/type.h"

namespace type {
struct Pointer : public Type {
  TYPE_FNS(Pointer);
  Pointer(Type const *t)
      : Type(Type::Flags{.is_default_initializable = 1,
                         .is_copyable              = 1,
                         .is_movable               = 1,
                         .has_destructor           = 0}),
        pointee(t) {}

  bool is_big() const override { return false; }
  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  Type const *pointee;
};

// Like Pointer but allows indexing and pointer arithmetic.
struct BufferPointer : public Pointer {
  BufferPointer() = delete;

  bool is_big() const override { return false; }
  void WriteTo(std::string *result) const override;
  BufferPointer(Type const *t) : Pointer(t) {}
};

Pointer const *Ptr(Type const *t);
BufferPointer const *BufPtr(Type const *t);
}  // namespace type
#endif  // ICARUS_TYPE_POINTER_H
