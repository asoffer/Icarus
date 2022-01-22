#include "backend/type.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/type.h"

namespace backend {
namespace {

struct LlvmTypeVisitor {
  using signature = llvm::Type *();

  explicit LlvmTypeVisitor(llvm::LLVMContext &context) : context_(context) {}

  llvm::Type *operator()(type::Type t) {
    return t.visit<LlvmTypeVisitor>(*this);
  }

  llvm::Type *operator()(auto const *t) {
    constexpr auto meta_type = base::meta<std::decay_t<decltype(*t)>>;
    if constexpr (meta_type == base::meta<type::Array>) {
      return llvm::ArrayType::get((*this)(t->data_type()), t->length().value());
    } else if constexpr (meta_type == base::meta<type::Enum>) {
      return llvm::Type::getInt64Ty(context_);
    } else if constexpr (meta_type == base::meta<type::Flags>) {
      return llvm::Type::getInt64Ty(context_);
    } else if constexpr (meta_type == base::meta<type::Primitive>) {
      return t->Apply([this]<typename T>() { return LlvmType<T>(context_); });
    } else if constexpr (meta_type == base::meta<type::Pointer> or
                         meta_type == base::meta<type::BufferPointer>) {
      return (*this)(t->pointee())->getPointerTo();
    } else if constexpr (meta_type == base::meta<type::Function>) {
      // TODO: Icarus uses "function types" in a way that can be confused with
      // LLVM. What LLVM calls a function-type, Icarus would call a constant
      // function type. Non-constant function types in Icarus are function
      // pointer types in LLVM. In other words, there is a type-level
      // distinction in LLVM that is a qualifier-level distinction in Icarus. We
      // don't actually take this into account yet.
      std::vector<llvm::Type *> param_types;
      for (auto const &p : t->params()) {
        if (p.value.constant()) { continue; }
        param_types.push_back((*this)(p.value.type()));
      }

      //  If an Icarus function has exactly one return value and it fits in a
      //  register, we use the function types return value. Otherwise, we use
      //  output parameter pointers.
      //
      //  TODO: We could maybe do better than this by packing two `i32`s into a
      //  single register on 64-bit architectures. We could also try to pick one
      //  return type that does fit in a rgeister and use that.
      auto const output_span = t->return_types();
      if (output_span.size() != 1 or output_span[0].get()->is_big()) {
        for (auto out_type : output_span) {
          param_types.push_back((*this)(out_type)->getPointerTo());
        }
        return llvm::FunctionType::get(llvm::Type::getVoidTy(context_),
                                       param_types, false);
      } else {
        return llvm::FunctionType::get((*this)(output_span[0]), param_types,
                                       false);
      }

    } else {
      NOT_YET();
    }
  }

 private:
  llvm::LLVMContext &context_;
};

}  // namespace

llvm::Type *ToLlvmType(type::Type t, llvm::LLVMContext &context) {
  LlvmTypeVisitor v(context);
  return v(t);
}

}  // namespace backend
