#ifndef ICARUS_TYPE_POINTER_H
#define ICARUS_TYPE_POINTER_H

#include "type.h"

#ifdef ICARUS_USE_LLVM
namespace llvm {
class PointerType;
} // namespace llvm
#endif // ICARUS_USE_LLVM

namespace type {
struct Pointer : public Type {
  TYPE_FNS(Pointer);
  Pointer(const Type *t) : pointee(t) {}
  const Type *pointee;

#ifdef ICARUS_USE_LLVM
  llvm::PointerType *llvm_ptr(llvm::LLVMContext &) const;
#endif // ICARUS_USE_LLVM
};

const Pointer *Ptr(const Type *t);
} // namespace type
#endif // ICARUS_TYPE_POINTER_H
