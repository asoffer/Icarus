#include "Type.h"
#include <iostream>

extern llvm::Module *global_module;

// This method allocates stack space for each particular type.

llvm::Value *Function::allocate() const {
  // TODO for now functions are treated as constant, and don't need to be
  // declared in a scope.
  //
  // What happens if you try to reassign? This almost certainly leads to a
  // bug.
  return llvm::Function::Create(static_cast<llvm::FunctionType *>(llvm_type),
                                llvm::Function::ExternalLinkage, "",
                                global_module);
}

llvm::Value *Array::allocate() const {
  auto alloc = builder.CreateAlloca(*this);
  alloc->setName("tmp_array");
  return alloc;
}

llvm::Value *Tuple::allocate() const {
  return nullptr; // TODO
}
