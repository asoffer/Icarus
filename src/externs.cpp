#include "AST.h"
#include "ErrorLog.h"

llvm::Module* global_module;
llvm::Function* global_function;
llvm::IRBuilder<> builder(llvm::getGlobalContext());

// TODO Only generate these if they are necessary
//
// TODO Reduce the dependency on the C standard library. This probably means
// writing platform-specific assembly.
namespace cstdlib {
  llvm::Constant* malloc;
  llvm::Constant* printf;
  llvm::Constant* putchar;
  llvm::Constant* puts;
  llvm::Value* format_d;
  llvm::Value* format_f;
  llvm::Value* format_s;
}  // namespace cstdlib

ErrorLog error_log;
