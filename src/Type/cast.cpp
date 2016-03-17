#include "Type.h"
#include "Scope.h"

llvm::Value* Primitive::call_cast(llvm::Value* val, Type* to_type) {
  if (this == Bool) {
    if (to_type == Int || to_type == Uint) {
      return builder.CreateZExt(val, *to_type, "ext_val");

    } else if (to_type == Real) {
      return builder.CreateUIToFP(val, *to_type, "ext_val");
    }

  } else if (this == Int) {
    if (to_type == Real) {
      return builder.CreateSIToFP(val, *to_type, "fp_val");

    } else if (to_type == Uint) {
      return val;
    }

  } else if (this == Uint) {
    if (to_type == Real) {
      return builder.CreateUIToFP(val, *to_type, "fp_val");

    } else if (to_type == Int) {
      // TODO is this actually going to be allowed?
      return val;
    }
  }

  return nullptr;
}

llvm::Value *Pointer::call_cast(llvm::Value *val, Type *to_type) {
  return nullptr;
}
llvm::Value *Tuple::call_cast(llvm::Value *val, Type *to_type) {
  return nullptr;
}
llvm::Value *Function::call_cast(llvm::Value *val, Type *to_type) {
  return nullptr;
}
llvm::Value *Array::call_cast(llvm::Value *val, Type *to_type) {
  return nullptr;
}
llvm::Value *Structure::call_cast(llvm::Value *val, Type *to_type) {
  return nullptr;
}
