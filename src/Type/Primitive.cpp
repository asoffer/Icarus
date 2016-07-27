#ifndef ICARUS_UNITY
#include "Type.h"
#endif

Primitive::Primitive(PrimType pt) : type_(pt) {
  switch (type_) {
  case PrimType::Bool:
    llvm_type = llvm::Type::getInt1Ty(llvm::getGlobalContext());
    break;
  case PrimType::Char:
    llvm_type = llvm::Type::getInt8Ty(llvm::getGlobalContext());
    break;
  case PrimType::Int:
    llvm_type = llvm::Type::getInt64Ty(llvm::getGlobalContext());
    break;
  case PrimType::Real:
    llvm_type = llvm::Type::getDoubleTy(llvm::getGlobalContext());
    break;
  case PrimType::Uint16:
    llvm_type = llvm::Type::getInt16Ty(llvm::getGlobalContext());
    break;
  case PrimType::Uint32:
    llvm_type = llvm::Type::getInt32Ty(llvm::getGlobalContext());
    break;
  case PrimType::Uint:
    llvm_type = llvm::Type::getInt64Ty(llvm::getGlobalContext());
    break;
  case PrimType::Void:
    llvm_type = llvm::Type::getVoidTy(llvm::getGlobalContext());
    break;
  default: llvm_type = nullptr;
  }
}

size_t Primitive::alignment() const {
  switch (type_) {
  case PrimType::Bool:
  case PrimType::Char: return 1;
  case PrimType::Uint16: return 2;
  case PrimType::Uint32: return 4;
  case PrimType::Int:
  case PrimType::Uint:
  case PrimType::Real: return 8;
  case PrimType::Void: return 1;
  case PrimType::Type: return 8;
  // TODO There's a difference between what we want when we use this at
  // compile-time and when we want the value at run-time

  default: UNREACHABLE;
  }
}

size_t Primitive::bytes() const {
  switch (type_) {
  case PrimType::Bool:
  case PrimType::Char: return 1;
  case PrimType::Uint16: return 2;
  case PrimType::Uint32: return 4;
  case PrimType::Int:
  case PrimType::Uint:
  case PrimType::Real: return 8;
  case PrimType::Void: return 0;
  case PrimType::Type: return 8;
  // TODO There's a difference between what we want when we use this at
  // compile-time and when we want the value at run-time

  default: UNREACHABLE;
  }
}
