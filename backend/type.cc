#include "backend/type.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/struct.h"
#include "type/type.h"
#include "type/visitor.h"

namespace backend {
namespace {
// TODO: Const visitor?
struct LlvmTypeVisitor : type::Visitor<llvm::Type *()> {
  explicit LlvmTypeVisitor(llvm::LLVMContext &context) : context_(context) {}

  llvm::Type *Visit(type::Type const *t) {
    return type::SingleVisitor<llvm::Type *()>::Visit(t);
  }

  llvm::Type *Visit(type::Array const *t) override {
    return llvm::ArrayType::get(Visit(t->data_type()), t->length());
  }

  llvm::Type *Visit(type::Enum const *t) override {
    return llvm::Type::getInt64Ty(context_);
  }

  llvm::Type *Visit(type::Flags const *t) override {
    return llvm::Type::getInt64Ty(context_);
  }

  // TODO: Icarus uses "function types" in a way that can be confused with LLVM.
  // What LLVM calls a function-type, Icarus would call a constant function
  // type. Non-constant function types in Icarus are function pointer types in
  // LLVM. In other words, there is a type-level distinction in LLVM that is a
  // qualifier-level distinction in Icarus. We don't actually take this into
  // account yet.
  llvm::Type *Visit(type::Function const *t) override {
    std::vector<llvm::Type *> param_types;
    for (auto const &p : t->params()) {
      if (p.value.constant()) { continue; }
      param_types.push_back(Visit(p.value.type()));
    }

    //  If an Icarus function has exactly one return value and it fits in a
    //  register, we use the function types return value. Otherwise, we use
    //  output parameter pointers.
    //
    //  TODO: We could maybe do better than this by packing two `int32`s into a
    //  single register on 64-bit architectures. We could also try to pick one
    //  return type that does fit in a rgeister and use that.
    auto const output_span = t->output();
    if (output_span.size() != 1 or output_span[0]->is_big()) {
      for (auto const *out_type : output_span) {
        param_types.push_back(Visit(out_type)->getPointerTo());
      }
      return llvm::FunctionType::get(llvm::Type::getVoidTy(context_),
                                     param_types, false);
    } else {
      return llvm::FunctionType::get(Visit(output_span[0]), param_types, false);
    }
  }

  llvm::Type *Visit(type::Pointer const *t) override {
    // Works for both pointers and buffer-pointers.
    return Visit(t->pointee())->getPointerTo();
  }

  llvm::Type *Visit(type::Primitive const *t) override {
    llvm::Type *result;
    t->Apply([&result, this](auto metatype) {
      result = LlvmType<typename decltype(metatype)::type>(context_);
    });
    return result;
  }

  llvm::Type *Visit(type::Struct const *t) override { NOT_YET(); }
  llvm::Type *Visit(type::Tuple const *t) override { NOT_YET(); }

 private:
  llvm::LLVMContext &context_;
};

}  // namespace

llvm::Type *ToLlvmType(type::Type const *t, llvm::LLVMContext &context) {
  return LlvmTypeVisitor(context).Visit(t);
}

}  // namespace backend
