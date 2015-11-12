#include "AST.h"
#include "ErrorLog.h"

llvm::Module* global_module;
llvm::Function* global_function;
llvm::Constant* global_print_char;
llvm::IRBuilder<> builder(llvm::getGlobalContext());

ErrorLog error_log;
