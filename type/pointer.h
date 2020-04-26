#ifndef ICARUS_TYPE_POINTER_H
#define ICARUS_TYPE_POINTER_H

#include "type/type.h"

namespace type {

// `Pointer` is a type representing the address of an object of the given
// pointed-to type (the `pointee`).
struct Pointer : Type {
  friend Pointer const *Ptr(Type const *t);

  ~Pointer() override {}

  void WriteTo(std::string *buf) const override;
  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  bool is_big() const override { return false; }
  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  constexpr Type const *pointee() const { return pointee_; }

 protected:
  Pointer(Type const *t)
      : Type(Type::Flags{.is_default_initializable = 1,
                         .is_copyable              = 1,
                         .is_movable               = 1,
                         .has_destructor           = 0}),
        pointee_(t) {}

 private:
  Type const *pointee_;
};

// `BufferPointer` is a type representing the address of an object, inside an
// a contiguous block of objects of that type. It is similar to `Pointer`, but
// it also supports arithmetic. `BufferPointer`s are implicitly convertible to
// `Pointer`s with the same `pointee` type.
struct BufferPointer : Pointer {
  friend BufferPointer const *BufPtr(Type const *t);

  ~BufferPointer() override {}

  bool is_big() const override { return false; }
  void WriteTo(std::string *result) const override;

 private:
  BufferPointer() = delete;
  BufferPointer(Type const *t) : Pointer(t) {}
};

Pointer const *Ptr(Type const *t);
BufferPointer const *BufPtr(Type const *t);

}  // namespace type
#endif  // ICARUS_TYPE_POINTER_H
