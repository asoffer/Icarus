#ifndef ICARUS_UNITY
#include "Type.h"
#endif

extern llvm::Module *global_module;

llvm::Value *Type::allocate() const { 
  assert(this != Void);

  return builder.CreateAlloca(*this); }

// This method allocates stack space for each particular type.

llvm::Value *Function::allocate() const {
  return builder.CreateAlloca(*Ptr(const_cast<Function *>(this)));
//  return llvm::Function::Create((llvm::FunctionType *)llvm_type,
//                                llvm::Function::ExternalLinkage, "__anon.fn",
//                                global_module);
}

llvm::Value *Array::allocate() const {
  auto alloc = builder.CreateAlloca(*this);
  if (!fixed_length) { alloc->setName("tmp_array"); }
  return alloc;
}

llvm::Value *Tuple::allocate() const { NOT_YET; }
