#include "AST.h"

namespace AST {
  llvm::IRBuilder<> builder(llvm::getGlobalContext());
  size_t function_counter = 0;
}  // namespace AST
