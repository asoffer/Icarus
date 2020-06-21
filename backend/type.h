#ifndef ICARUS_BACKEND_TYPE_H
#define ICARUS_BACKEND_TYPE_H

#include "base/meta.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "type/type.h"

namespace backend {

llvm::Type *ToLlvmType(type::Type const *, llvm::LLVMContext &);

template <typename T>
llvm::Type *LlvmType(llvm::LLVMContext &context) {
  if constexpr (base::meta<T> == base::meta<bool>) {
    return llvm::Type::getInt1Ty(context);
  } else if constexpr (base::meta<T> == base::meta<uint8_t> or
                       base::meta<T> == base::meta<int8_t>) {
    return llvm::Type::getInt8Ty(context);
  } else if constexpr (base::meta<T> == base::meta<uint16_t> or
                       base::meta<T> == base::meta<int16_t>) {
    return llvm::Type::getInt16Ty(context);
  } else if constexpr (base::meta<T> == base::meta<uint32_t> or
                       base::meta<T> == base::meta<int32_t>) {
    return llvm::Type::getInt32Ty(context);
  } else if constexpr (base::meta<T> == base::meta<uint64_t> or
                       base::meta<T> == base::meta<int64_t>) {
    return llvm::Type::getInt64Ty(context);
  } else if constexpr (base::meta<T> == base::meta<float>) {
    return llvm::Type::getFloatTy(context);
  } else if constexpr (base::meta<T> == base::meta<double>) {
    return llvm::Type::getDoubleTy(context);
  } else {
    NOT_YET();
  }
}

}  // namespace backend

#endif // ICARUS_BACKEND_TYPE_H
