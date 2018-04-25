#ifndef ICARUS_TYPE_POINTER_H
#define ICARUS_TYPE_POINTER_H

#include "type.h"

namespace llvm {
class PointerType;
} // namespace llvm

namespace type {
struct Pointer : public Type {
  TYPE_FNS(Pointer);
  Pointer(const Type *t) : pointee(t) {}
  const Type *pointee;

  llvm::PointerType *llvm_ptr(llvm::LLVMContext &) const;
};

const Pointer *Ptr(const Type *t);
} // namespace type
#endif // ICARUS_TYPE_POINTER_H
