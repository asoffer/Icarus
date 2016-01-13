#ifndef ICARUS_TYPE_OPS_H
#define ICARUS_TYPE_OPS_H

#include "Language.h"

llvm::Value* Primitive::call_add(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) {
  if (this == get_int() || this == get_uint()) {
    return bldr.CreateAdd(lhs, rhs, "add");

  } else if (this == get_real()) {
    return bldr.CreateFAdd(lhs, rhs, "fadd");

  } else {
    return nullptr;
  }
}

llvm::Value* Primitive::call_sub(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) {
  if (this == get_int() || this == get_uint()) {
    return bldr.CreateSub(lhs, rhs, "sub");

  } else if (this == get_real()) {
    return bldr.CreateFSub(lhs, rhs, "fsub");

  } else {
    return nullptr;
  }
}

llvm::Value* Primitive::call_mul(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) {
  if (this == get_int() || this == get_uint()) {
    return bldr.CreateMul(lhs, rhs, "mul");

  } else if (this == get_real()) {
    return bldr.CreateFMul(lhs, rhs, "fmul");

  } else {
    return nullptr;
  }
}

llvm::Value* Primitive::call_div(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) {
  if (this == get_int()) {
    return bldr.CreateSDiv(lhs, rhs, "sdiv");

  } else if (this == get_uint()) {
    return bldr.CreateUDiv(lhs, rhs, "udiv");

  } else if (this == get_real()) {
    return bldr.CreateFDiv(lhs, rhs, "fdiv");

  } else {
    return nullptr;
  }
}

llvm::Value* Primitive::call_mod(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) {
  if (this == get_int()) {
    return bldr.CreateSRem(lhs, rhs, "smod");

  } else if (this == get_uint()) {
    return bldr.CreateURem(lhs, rhs, "umod");

  } else if (this == get_real()) {
    return bldr.CreateFRem(lhs, rhs, "fmod");

  } else {
    return nullptr;
  }
}

llvm::Value* Array::call_add(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Array::call_sub(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Array::call_mul(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Array::call_div(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Array::call_mod(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }

llvm::Value* Tuple::call_add(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Tuple::call_sub(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Tuple::call_mul(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Tuple::call_div(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Tuple::call_mod(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }

llvm::Value* Pointer::call_add(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Pointer::call_sub(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Pointer::call_mul(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Pointer::call_div(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Pointer::call_mod(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }

llvm::Value* Function::call_add(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Function::call_sub(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Function::call_mul(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Function::call_div(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* Function::call_mod(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }

llvm::Value* UserDefined::call_add(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* UserDefined::call_sub(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* UserDefined::call_mul(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* UserDefined::call_div(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
llvm::Value* UserDefined::call_mod(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }

#endif  // ICARUS_TYPE_OPS_H
