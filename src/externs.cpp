#include "AST.h"
#include "ErrorLog.h"

// Debug flag for parser. Turns on step-by-step iteration through the shifting and
// reducing.
bool DEBUG_PARSER = false;

llvm::Module* global_module;
llvm::Function* global_function;
llvm::IRBuilder<> global_builder(llvm::getGlobalContext());

// TODO Only generate these if they are necessary
//
// TODO Reduce the dependency on the C standard library. This probably means
// writing platform-specific assembly.
namespace cstdlib {
  llvm::Constant* free() {
    static llvm::Constant* free_ =
      global_module->getOrInsertFunction("free", 
          llvm::FunctionType::get(Type::get_void()->llvm(),
          { Type::get_pointer(Type::get_char())->llvm()}, false));
    return free_;
  }

  llvm::Constant* malloc() {
    static llvm::Constant* malloc_ =
      global_module->getOrInsertFunction("malloc",
          llvm::FunctionType::get(Type::get_pointer(Type::get_char())->llvm(),
            { Type::get_uint()->llvm() }, false));

    return malloc_;
  }
  
  llvm::Constant* printf() {
    static llvm::Constant* printf_ =
      global_module->getOrInsertFunction("printf",
          llvm::FunctionType::get(Type::get_int()->llvm(),
            { llvm::Type::getInt8PtrTy(llvm::getGlobalContext()) }, true));

    return printf_;
  }

  llvm::Constant* putchar() {
    static llvm::Constant* putchar_ =
      global_module->getOrInsertFunction("putchar",
          llvm::FunctionType::get(Type::get_int()->llvm(),
            { Type::get_char()->llvm() }, false));

    return putchar_;
  }

  llvm::Constant* puts() {
    static llvm::Constant* puts_ =
      global_module->getOrInsertFunction("puts",
          llvm::FunctionType::get(Type::get_int()->llvm(),
            { llvm::Type::getInt8PtrTy(llvm::getGlobalContext()) }, false));

    return puts_;
  }
}  // namespace cstdlib

namespace data {
  llvm::Value* const_int(size_t n, bool is_signed = false) {
    return llvm::ConstantInt::get(llvm::getGlobalContext(),
        llvm::APInt(32, n, is_signed));
  }

  llvm::Value* const_char(char c) {
    // TODO check safety of char cast
    return llvm::ConstantInt::get(llvm::getGlobalContext(),
        llvm::APInt(8, static_cast<size_t>(c), false));
  }

}  // namespace data

ErrorLog error_log;
