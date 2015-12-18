#include "Type.h"
#include <iostream>
extern llvm::Module* global_module;

// This method allocates stack space for each particular type.

llvm::Value* Array::allocate(llvm::IRBuilder<>& bldr) const {
  // TODO currently it doesn't matter if the length is technically
  // dynamic or not. We're doing no optimizations using this

  return bldr.CreateAlloca(Type::get_pointer(data_type())->llvm());
}

llvm::Value* Function::allocate(llvm::IRBuilder<>& bldr) const {
  // TODO for now functions are treated as constant, and don't need to be
  // declared in a scope.
  //
  // What happens if you try to reassign? This almost certainly leads to a
  // bug.
  return llvm::Function::Create(
      static_cast<llvm::FunctionType*>(llvm()),
      llvm::Function::ExternalLinkage, "", global_module);
}

llvm::Value* Pointer::allocate(llvm::IRBuilder<>& bldr) const {
  // TODO
  return nullptr;
}

llvm::Value* Primitive::allocate(llvm::IRBuilder<>& bldr) const {
  return bldr.CreateAlloca(llvm());
}

llvm::Value* Tuple::allocate(llvm::IRBuilder<>& bldr) const {
  // TODO
  return nullptr;
}

llvm::Value* UserDefined::allocate(llvm::IRBuilder<>& bldr) const {
  return bldr.CreateAlloca(llvm());
}
