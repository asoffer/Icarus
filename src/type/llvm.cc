#include "type/all.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

namespace type {
llvm::Type* Primitive::llvm(llvm::LLVMContext& ctx) const {
  switch (type_) {
  case PrimType::Err: UNREACHABLE();
  case PrimType::Unknown: UNREACHABLE();
  case PrimType::Type: return llvm::Type::getInt64Ty(ctx);
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
llvm::Type* Enum::llvm(llvm::LLVMContext& ctx) const { 
  // TODO make as wide as is necessary
  return llvm::Type::getInt32Ty(ctx);
}
llvm::Type* Function::llvm(llvm::LLVMContext& ctx) const {
  return llvm_fn(ctx);
}
llvm::Type* Pointer::llvm(llvm::LLVMContext &ctx) const {
  return pointee->llvm(ctx)->getPointerTo(0);
}
llvm::Type* Variant::llvm(llvm::LLVMContext& ctx) const { NOT_YET(); }
llvm::Type* Range::llvm(llvm::LLVMContext& ctx) const { UNREACHABLE(); }
llvm::Type* Slice::llvm(llvm::LLVMContext& ctx) const { UNREACHABLE(); }
llvm::Type* Scope::llvm(llvm::LLVMContext& ctx) const { UNREACHABLE(); }
llvm::Type* Struct::llvm(llvm::LLVMContext& ctx) const { NOT_YET(); }

llvm::FunctionType* Function::llvm_fn(llvm::LLVMContext& ctx) const {
  ASSERT_LE(output.size(), 1u);
  std::vector<llvm::Type*> llvm_inputs;
  llvm_inputs.reserve(input.size());
  for (auto* t : input) { llvm_inputs.push_back(t->llvm(ctx)); }
  return llvm::FunctionType::get(
      output.empty() ? llvm::Type::getVoidTy(ctx) : output[0]->llvm(ctx),
      llvm_inputs, false);
}
} // namespace type
