#include "AST.h"

llvm::Module* global_module;
llvm::Function* global_function;

namespace AST {
  size_t function_counter = 0;
}  // namespace AST
