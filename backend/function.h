#ifndef ICARUS_BACKEND_FUNCTION_H
#define ICARUS_BACKEND_FUNCTION_H

#include "compiler/module.h"
#include "ir/compiled_fn.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"

namespace backend {

llvm::Function *DeclareLlvmFunction(ir::CompiledFn const &fn,
                                    compiler::CompiledModule const &module,
                                    llvm::Module &llvm_module);

void EmitLlvmFunction(llvm::IRBuilder<> &builder, llvm::LLVMContext &context,
                      ir::CompiledFn const &fn, llvm::Function &llvm_fn);

}  // namespace backend

#endif  // ICARUS_BACKEND_FUNCTION_H
