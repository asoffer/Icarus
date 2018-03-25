#include "type/all.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

namespace type {
llvm::Type* Primitive::llvm(llvm::LLVMContext& ctx) const {
  switch (type_) {
  case PrimType::Err: UNREACHABLE();
  case PrimType::Unknown: UNREACHABLE();
  case PrimType::Type: NOT_YET();
  case PrimType::Void: return llvm::Type::getVoidTy(ctx);
  case PrimType::NullPtr: UNREACHABLE();
  case PrimType::EmptyArray: UNREACHABLE();
  case PrimType::Code: UNREACHABLE();
  case PrimType::Bool: return llvm::Type::getInt1Ty(ctx);
  case PrimType::Char: return llvm::Type::getInt8Ty(ctx);
  case PrimType::Int: return llvm::Type::getInt32Ty(ctx);
  case PrimType::Real: return llvm::Type::getDoubleTy(ctx); 
  case PrimType::String: NOT_YET();
  default: UNREACHABLE();
  }
}
llvm::Type* Array::llvm(llvm::LLVMContext& ctx) const { NOT_YET(); }
llvm::Type* Tuple::llvm(llvm::LLVMContext& ctx) const { NOT_YET(); }
llvm::Type* Enum::llvm(llvm::LLVMContext& ctx) const { NOT_YET(); }
llvm::Type* Function::llvm(llvm::LLVMContext& ctx) const { NOT_YET(); }
llvm::Type* Pointer::llvm(llvm::LLVMContext &ctx) const {
  return pointee->llvm(ctx)->getPointerTo(0);
}
llvm::Type* Variant::llvm(llvm::LLVMContext& ctx) const { NOT_YET(); }
llvm::Type* Range::llvm(llvm::LLVMContext& ctx) const { UNREACHABLE(); }
llvm::Type* Slice::llvm(llvm::LLVMContext& ctx) const { UNREACHABLE(); }
llvm::Type* Scope::llvm(llvm::LLVMContext& ctx) const { UNREACHABLE(); }
llvm::Type* Struct::llvm(llvm::LLVMContext& ctx) const { NOT_YET(); }
} // namespace type
