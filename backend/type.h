#ifndef ICARUS_BACKEND_TYPE_H
#define ICARUS_BACKEND_TYPE_H

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "type/type.h"

namespace backend {

llvm::Type *ToLlvmType(type::Type const *, llvm::LLVMContext &);

}  // namespace backend

#endif // ICARUS_BACKEND_TYPE_H
