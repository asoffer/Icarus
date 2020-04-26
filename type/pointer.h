#ifndef ICARUS_TYPE_POINTER_H
#define ICARUS_TYPE_POINTER_H

#include "type/type.h"

namespace type {
struct Pointer;

Pointer const *Ptr(Type const *t);

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

struct BufferPoniter;

BufferPointer const *BufPtr(Type const *t);

// Like Pointer but allows indexing and pointer arithmetic.
struct BufferPointer : Pointer {
  friend BufferPointer const *BufPtr(Type const *t);

  ~BufferPointer() override {}

  bool is_big() const override { return false; }
  void WriteTo(std::string *result) const override;

 private:
  BufferPointer() = delete;
  BufferPointer(Type const *t) : Pointer(t) {}
};

}  // namespace type
#endif  // ICARUS_TYPE_POINTER_H
