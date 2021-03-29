#ifndef ICARUS_BACKEND_LLVM_H
#define ICARUS_BACKEND_LLVM_H

#include "absl/container/flat_hash_map.h"
#include "backend/emit.h"
#include "backend/type.h"
#include "compiler/module.h"
#include "ir/compiled_fn.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"

namespace backend {

struct LlvmBackendTraits {
  using function_type    = llvm::Function;
  using basic_block_type = llvm::BasicBlock;
  using value_type       = llvm::Value;
  using module_type       = llvm::Module;
};

struct LlvmEmitter : Emitter<LlvmEmitter, LlvmBackendTraits> {
  using traits_type  = LlvmBackendTraits;
  using context_type = EmitContext<traits_type>;

  explicit LlvmEmitter(llvm::IRBuilder<> &builder, module_type *module);

  function_type *DeclareFunction(ir::CompiledFn const *fn,
                                 module::Linkage linkage,
                                 module_type &output_module);

  basic_block_type *DeclareBasicBlock(function_type &fn);

  void PrepareForStackAllocation(
      ir::CompiledFn const &fn,
      const absl::flat_hash_map<ir::BasicBlock const *, basic_block_type *>
          &block_map);

  void PrepareForBasicBlockAppend(basic_block_type *block);

  value_type *StackAllocate(type::Type t);

  bool EmitInstruction(ir::Inst const &instruction, context_type &context);

  void EmitBasicBlockJump(ir::BasicBlock const *block, context_type &context,
                          bool returns_void);

  llvm::IRBuilder<> &builder() { return builder_; }
  llvm::LLVMContext &context() { return context_; }

  template <typename T>
  value_type *Resolve(ir::RegOr<T> val, context_type &context) {
    if (val.is_reg()) {
      if (val.reg().is_arg()) {
        // TODO: Use getArg when you upgrade to llvm 11.0
        return builder_.GetInsertBlock()->getParent()->arg_begin() +
               val.reg().arg_value();
      }
      auto iter = context.registers.find(val.reg());
      if (iter == context.registers.end()) { return nullptr; }
      return iter->second;
    } else {
      if constexpr (std::is_integral_v<T>) {
        return llvm::Constant::getIntegerValue(
            LlvmType<T>(context_),
            llvm::APInt(sizeof(T) * CHAR_BIT,
                        static_cast<uint64_t>(val.value()),
                        std::is_signed_v<T>));
      } else if constexpr (base::meta<T> == base::meta<ir::Fn>) {
        switch (val.value().kind()) {
          case ir::Fn::Kind::Native: {
            return context.functions.at(val.value().native().get());
          } break;
          default: NOT_YET();
        }
      } else {
        NOT_YET(typeid(T).name());
      }
    }
  }

 private:
  llvm::IRBuilder<> &builder_;
  llvm::LLVMContext &context_;
};

}  // namespace backend

#endif  // ICARUS_BACKEND_LLVM_H
