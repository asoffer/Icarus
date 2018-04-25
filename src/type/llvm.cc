#include "type/all.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "architecture.h"

namespace type {
llvm::Type* Primitive::llvm(llvm::LLVMContext& ctx) const {
  switch (type_) {
  case PrimType::Err: UNREACHABLE();
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
llvm::Type* Array::llvm(llvm::LLVMContext& ctx) const {
  if (fixed_length) {
    return llvm::ArrayType::get(data_type->llvm(ctx), len);
  } else {
    return llvm::StructType::get(ctx, {llvm::Type::getInt64Ty(ctx),
                                       data_type->llvm(ctx)->getPointerTo(0)});
  }
}

llvm::Type* Enum::llvm(llvm::LLVMContext& ctx) const { 
  // TODO make as wide as is necessary
  return llvm::Type::getInt32Ty(ctx);
}
llvm::Type* Function::llvm(llvm::LLVMContext& ctx) const {
  return llvm_fn(ctx);
}
llvm::Type* Pointer::llvm(llvm::LLVMContext &ctx) const {
  return llvm_ptr(ctx);
}
llvm::PointerType* Pointer::llvm_ptr(llvm::LLVMContext &ctx) const {
  return pointee->llvm(ctx)->getPointerTo(0);
}
llvm::Type* Variant::llvm(llvm::LLVMContext& ctx) const {
  // TODO pass in information about the machine we're compiling to
  auto arch = Architecture::CompilingMachine();

  const Type* max_elem       = nullptr;
  size_t max_alignment = 0;
  for (const Type* v : variants_) {
    auto v_alignment = arch.alignment(v);
    if (max_alignment < v_alignment) {
      max_alignment = v_alignment;
      max_elem      = v;
    }
  }
  auto extra_bytes = arch.bytes(this) - arch.bytes(max_elem) - 8;
  if (extra_bytes == 0) {
    return llvm::StructType::get(
        ctx, {llvm::Type::getInt64Ty(ctx), max_elem->llvm(ctx)});
  } else {
    return llvm::StructType::get(
        ctx,
        {llvm::Type::getInt64Ty(ctx), max_elem->llvm(ctx),
         llvm::ArrayType::get(llvm::Type::getInt8Ty(ctx),
                              arch.bytes(this) - arch.bytes(max_elem) - 8)});
  }
}

llvm::Type* Range::llvm(llvm::LLVMContext& ctx) const { UNREACHABLE(); }
llvm::Type* Slice::llvm(llvm::LLVMContext& ctx) const { UNREACHABLE(); }
llvm::Type* Scope::llvm(llvm::LLVMContext& ctx) const { UNREACHABLE(); }
llvm::Type* Struct::llvm(llvm::LLVMContext& ctx) const {
  std::vector<llvm::Type*> llvm_types;
  llvm_types.reserve(fields_.size());
  for (const auto& f : fields_) { llvm_types.push_back(f.type->llvm(ctx)); }
  return llvm::StructType::get(ctx, llvm_types);
}

llvm::FunctionType* Function::llvm_fn(llvm::LLVMContext& ctx) const {
  std::vector<llvm::Type*> llvm_inputs;
  llvm_inputs.reserve(input.size());
  for (auto* t : input) {
    auto* llvm_type = t->llvm(ctx);
    if (t->is_big()) { llvm_type = llvm_type->getPointerTo(0); }
    llvm_inputs.push_back(llvm_type);
  }
  switch (output.size()) {
    case 0:
      return llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), llvm_inputs,
                                     false);
    case 1:
      return llvm::FunctionType::get(output[0]->llvm(ctx), llvm_inputs, false);
    default: {
      for (auto* t : output) {
        auto* llvm_type = t->llvm(ctx);
        if (t->is_big()) { llvm_type = llvm_type->getPointerTo(0); }
        llvm_inputs.push_back(llvm_type);
      }
      return llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), llvm_inputs,
                                     false);
    } break;
  }
}
} // namespace type
