#ifndef ICARUS_TYPE_POINTER_H
#define ICARUS_TYPE_POINTER_H

#include "type.h"

#ifdef ICARUS_USE_LLVM
namespace llvm {
class PointerType;
}  // namespace llvm
#endif  // ICARUS_USE_LLVM

namespace type {
struct Pointer : public Type {
  TYPE_FNS(Pointer);
  Pointer(Type const *t) : pointee(t) {}
  Type const *pointee;

#ifdef ICARUS_USE_LLVM
  llvm::PointerType *llvm_ptr(llvm::LLVMContext &) const;
#endif  // ICARUS_USE_LLVM
};

// Like Pointer but allows indexing and pointer arithmetic.
struct BufferPointer : public Pointer {
  BufferPointer() = delete;
  char *WriteTo(char *buf) const override;
  size_t string_size() const override;
  BufferPointer(Type const *t) : Pointer(t) {}
};

Pointer const *Ptr(Type const *t);
BufferPointer const *BufPtr(Type const *t);
}  // namespace type
#endif  // ICARUS_TYPE_POINTER_H
