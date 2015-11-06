#include "AST.h"
#include "ErrorLog.h"

llvm::Module* global_module;
llvm::Function* global_function;
llvm::IRBuilder<> builder(llvm::getGlobalContext());

ErrorLog error_log;
