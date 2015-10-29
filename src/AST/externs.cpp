#include "AST.h"

llvm::Module* global_module;
llvm::Function* global_function;
llvm::IRBuilder<> builder(llvm::getGlobalContext());
namespace AST {
  size_t function_counter = 0;
}  // namespace AST
