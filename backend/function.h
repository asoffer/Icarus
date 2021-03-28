#ifndef ICARUS_BACKEND_FUNCTION_H
#define ICARUS_BACKEND_FUNCTION_H

#include "absl/container/flat_hash_map.h"
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
                      ir::CompiledFn const &fn,
                      absl::flat_hash_map<ir::CompiledFn const *,
                                          llvm::Function *> const &fn_map);

}  // namespace backend

#endif  // ICARUS_BACKEND_FUNCTION_H
