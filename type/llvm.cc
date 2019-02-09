#ifdef ICARUS_USE_LLVM
#include "architecture.h"
#include "llvm/ir/DerivedTypes.h"
#include "llvm/ir/LLVMContext.h"
#include "llvm/ir/Type.h"
#include "type/all.h"

namespace type {
llvm::Type* Primitive::llvm(llvm::LLVMContext& ctx) const {
  switch (type_) {
    case PrimType::Type: return llvm::Type::getInt64Ty(ctx);
    case PrimType::NullPtr: UNREACHABLE();
    case PrimType::EmptyArray: UNREACHABLE();
    case PrimType::Code: UNREACHABLE();
    case PrimType::Bool: return llvm::Type::getInt1Ty(ctx);
    case PrimType::Int8: return llvm::Type::getInt8Ty(ctx);
    case PrimType::Int16: return llvm::Type::getInt16Ty(ctx);
    case PrimType::Int32: return llvm::Type::getInt32Ty(ctx);
    case PrimType::Int64: return llvm::Type::getInt64Ty(ctx);
    case PrimType::Nat8: return llvm::Type::getInt8Ty(ctx);
    case PrimType::Nat16: return llvm::Type::getInt16Ty(ctx);
    case PrimType::Nat32: return llvm::Type::getInt32Ty(ctx);
    case PrimType::Nat64: return llvm::Type::getInt64Ty(ctx);
    case PrimType::Float32: return llvm::Type::getFloatTy(ctx);
    case PrimType::Float64: return llvm::Type::getDoubleTy(ctx);
    default: UNREACHABLE(to_string());
  }
}
llvm::Type* Array::llvm(llvm::LLVMContext& ctx) const {
  return llvm::ArrayType::get(data_type->llvm(ctx), len);
}

llvm::Type* Enum::llvm(llvm::LLVMContext& ctx) const {
  // TODO make as wide as is necessary
  return llvm::Type::getInt32Ty(ctx);
}
llvm::Type* Flags::llvm(llvm::LLVMContext& ctx) const {
  // TODO make as wide as is necessary
  return llvm::Type::getInt32Ty(ctx);
}
llvm::Type* Function::llvm(llvm::LLVMContext& ctx) const {
  return llvm_fn(ctx);
}
llvm::Type* Pointer::llvm(llvm::LLVMContext& ctx) const {
  return llvm_ptr(ctx);
}
llvm::PointerType* Pointer::llvm_ptr(llvm::LLVMContext& ctx) const {
  return pointee->llvm(ctx)->getPointerTo(0);
}
llvm::Type* Variant::llvm(llvm::LLVMContext& ctx) const {
  // TODO pass in information about the machine we're compiling to
  auto arch = Architecture::CompilingMachine();

  const Type* max_elem = nullptr;
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
      if (output[0]->is_big()) {
        llvm_inputs.push_back(output[0]->llvm(ctx)->getPointerTo(0));
        return llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), llvm_inputs,
                                       false);
      } else {
        return llvm::FunctionType::get(output[0]->llvm(ctx), llvm_inputs,
                                       false);
      }
    default: {
      for (auto* t : output) {
        auto* llvm_type = t->llvm(ctx)->getPointerTo(0);
        llvm_inputs.push_back(llvm_type);
      }
      return llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), llvm_inputs,
                                     false);
    } break;
  }
}
}  // namespace type
#endif  // ICARUS_USE_LLVM
