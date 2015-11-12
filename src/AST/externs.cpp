#include "AST.h"
#include "ErrorLog.h"

llvm::Module* global_module;
llvm::Function* global_function;
llvm::IRBuilder<> builder(llvm::getGlobalContext());

llvm::Constant* external_putchar;
llvm::Constant* external_printf;

ErrorLog error_log;
