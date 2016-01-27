#include "Type.h"
#include "Scope.h"

llvm::Value* Primitive::call_cast(llvm::IRBuilder<>& bldr, llvm::Value* val, Type* to_type) {
  if (this == get_bool()) {
    if (to_type == get_int() || to_type == get_uint()) {
      return bldr.CreateZExt(val, *to_type, "ext_val");

    } else if (to_type == get_real()) {
      return bldr.CreateUIToFP(val, *to_type, "ext_val");
    }

  } else if (this == get_int()) {
    if (to_type == get_real()) {
      return bldr.CreateSIToFP(val, *to_type, "fp_val");

    } else if (to_type == get_uint()) {
      return val;
    }

  } else if (this == get_uint()) {
    if (to_type == get_real()) {
      return bldr.CreateUIToFP(val, *to_type, "fp_val");

    } else if (to_type == get_int()) {
      // TODO is this actually going to be allowed?
      return val;
    }
  }

  return nullptr;
}

llvm::Value* Pointer::call_cast(llvm::IRBuilder<>& bldr, llvm::Value* val, Type* to_type) { return nullptr; }
llvm::Value* Tuple::call_cast(llvm::IRBuilder<>& bldr, llvm::Value* val, Type* to_type) { return nullptr; }
llvm::Value* Function::call_cast(llvm::IRBuilder<>& bldr, llvm::Value* val, Type* to_type) { return nullptr; }
llvm::Value* Array::call_cast(llvm::IRBuilder<>& bldr, llvm::Value* val, Type* to_type) { return nullptr; }
llvm::Value* UserDefined::call_cast(llvm::IRBuilder<>& bldr, llvm::Value* val, Type* to_type) { return nullptr; }
