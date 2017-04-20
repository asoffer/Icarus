#include "type.h"

extern llvm::LLVMContext GlobalContext;

Primitive::Primitive(PrimType pt) : type_(pt) {
  switch (type_) {
  case PrimType::Bool:
    llvm_type = llvm::Type::getInt1Ty(GlobalContext);
    break;
  case PrimType::Char:
    llvm_type = llvm::Type::getInt8Ty(GlobalContext);
    break;
  case PrimType::Int:
    llvm_type = llvm::Type::getInt64Ty(GlobalContext);
    break;
  case PrimType::Real:
    llvm_type = llvm::Type::getDoubleTy(GlobalContext);
    break;
  case PrimType::U16:
    llvm_type = llvm::Type::getInt16Ty(GlobalContext);
    break;
  case PrimType::U32:
    llvm_type = llvm::Type::getInt32Ty(GlobalContext);
    break;
  case PrimType::Uint:
    llvm_type = llvm::Type::getInt64Ty(GlobalContext);
    break;
  case PrimType::Void:
    llvm_type = llvm::Type::getVoidTy(GlobalContext);
    break;
  case PrimType::String:
    llvm_type =
        llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(GlobalContext));
    break;

  default: llvm_type = nullptr;
  }
}

size_t Primitive::alignment() const {
  switch (type_) {
  case PrimType::Bool:
  case PrimType::Char: return 1;
  case PrimType::U16: return 2;
  case PrimType::U32: return 4;
  case PrimType::Int:
  case PrimType::Uint:
  case PrimType::Real: return 8;
  case PrimType::Void: return 1;
  case PrimType::Type: return 8;
  case PrimType::Code: return 8;
  case PrimType::String: return 8;
  // TODO There's a difference between what we want when we use this at
  // compile-time and when we want the value at run-time

  default: UNREACHABLE;
  }
}

size_t Primitive::bytes() const {
  switch (type_) {
  case PrimType::Bool:
  case PrimType::Char: return 1;
  case PrimType::U16: return 2;
  case PrimType::U32: return 4;
  case PrimType::Int:
  case PrimType::Uint:
  case PrimType::Real: return 8;
  case PrimType::Void: return 0;
  case PrimType::Type: return 8;
  case PrimType::String: return 8;
  case PrimType::Code: return 8;
                       
  // TODO There's a difference between what we want when we use this at
  // compile-time and when we want the value at run-time

  default: UNREACHABLE;
  }
}

std::string Primitive::to_string() const {
  switch (type_) {
  case PrimType::Err: return "Err";
  case PrimType::Unknown: return "???";
  case PrimType::Bool: return "bool";
  case PrimType::Char: return "char";
  case PrimType::Code: return "code";
  case PrimType::Int: return "int";
  case PrimType::Real: return "real";
  case PrimType::Type: return "type";
  case PrimType::Uint: return "uint";
  case PrimType::Void: return "void";
  case PrimType::NullPtr: return "null";
  case PrimType::U16: return "uint16";
  case PrimType::U32: return "uint32";
  case PrimType::String: return "string";
  default: UNREACHABLE;
  }
}

bool Primitive::private_has_vars() { return false; }
