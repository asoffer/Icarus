#ifndef ICARUS_TYPE_OPS_H
#define ICARUS_TYPE_OPS_H

#include "Language.h"

llvm::Value* TypeSystem::Primitive::call_add(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) {
  if (this == Int || this == Uint) {
    return bldr.CreateAdd(lhs, rhs, "add");

  } else if (this == Real) {
    return bldr.CreateFAdd(lhs, rhs, "fadd");

  } else {
    return nullptr;
  }
}

llvm::Value* TypeSystem::Primitive::call_sub(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) {
  if (this == Int || this == Uint) {
    return bldr.CreateSub(lhs, rhs, "sub");

  } else if (this == Real) {
    return bldr.CreateFSub(lhs, rhs, "fsub");

  } else {
    return nullptr;
  }
}

llvm::Value* TypeSystem::Primitive::call_mul(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) {
  if (this == Int || this == Uint) {
    return bldr.CreateMul(lhs, rhs, "mul");

  } else if (this == Real) {
    return bldr.CreateFMul(lhs, rhs, "fmul");

  } else {
    return nullptr;
  }
}

llvm::Value* TypeSystem::Primitive::call_div(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) {
  if (this == Int) {
    return bldr.CreateSDiv(lhs, rhs, "sdiv");

  } else if (this == Uint) {
    return bldr.CreateUDiv(lhs, rhs, "udiv");

  } else if (this == Real) {
    return bldr.CreateFDiv(lhs, rhs, "fdiv");

  } else {
    return nullptr;
  }
}

llvm::Value* TypeSystem::Primitive::call_mod(llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) {
  if (this == Int) {
    return bldr.CreateSRem(lhs, rhs, "smod");

  } else if (this == Uint) {
    return bldr.CreateURem(lhs, rhs, "umod");

  } else if (this == Real) {
    return bldr.CreateFRem(lhs, rhs, "fmod");

  } else {
    return nullptr;
  }
}

llvm::Value* TypeSystem::Primitive::call_neg(llvm::IRBuilder<>& bldr, llvm::Value* operand) {
  if (this == Int) {
    return bldr.CreateNeg(operand, "neg");

  } else if (this == Real) {
    return bldr.CreateFNeg(operand, "fneg");

  } else {
    return nullptr;
  }
}

llvm::Value* TypeSystem::Primitive::call_not(llvm::IRBuilder<>& bldr, llvm::Value* operand) {
  if (this == Char) {
    return bldr.CreateNot(operand, "not");

  } else {
    return nullptr;
  }
}

#define BINARY_OPERATOR_MACRO(op, symbol, prec, assoc) \
  llvm::Value* TYPE::call_##op (llvm::IRBuilder<>& bldr, llvm::Value* lhs, llvm::Value* rhs) { return nullptr; }
#define LEFT_UNARY_OPERATOR_MACRO(op) \
  llvm::Value* TYPE::call_##op (llvm::IRBuilder<>& bldr, llvm::Value* operand) { return nullptr; }


#define TYPE Array 
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE Tuple
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE Pointer
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE Function
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE UserDefined
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE

#define TYPE Enum
#include "config/left_unary_operators.conf"
#include "config/binary_operators.conf"
#undef TYPE


#undef CHAIN_OPERATOR_MACRO
#undef BINARY_OPERATOR_MACRO

#endif  // ICARUS_TYPE_OPS_H
