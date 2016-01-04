#include "AST.h"
#include "ErrorLog.h"

#include <map>
#include <queue>

// Debug flags and their default values
namespace debug {
  // Turns on step-by-step iteration through the shifting and reducing.
  bool parser = false;
}

std::map<std::string, StmtsPtr> ast_map;
std::queue<std::string> file_queue;

llvm::Module* global_module;

std::map<std::string, llvm::Value*> global_strings;

ErrorLog error_log;

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

  llvm::Constant* calloc() {
    static llvm::Constant* calloc_ =
      global_module->getOrInsertFunction("calloc",
          llvm::FunctionType::get(Type::get_pointer(Type::get_char())->llvm(),
            { Type::get_uint()->llvm() }, false));

    return calloc_;
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
  llvm::Value* const_int(llvm::IRBuilder<>& bldr, int n, bool is_signed = false) {
#ifdef DEBUG
    if (n < 0 && !is_signed) {
      std::cerr << "FATAL: Unsigned negative integer!" << std::endl;
    }
#endif
    if (n >= 0) {
      return llvm::ConstantInt::get(llvm::getGlobalContext(),
          llvm::APInt(32, static_cast<size_t>(n), is_signed));
    } else {
      return bldr.CreateSub(
          llvm::ConstantInt::get(
            llvm::getGlobalContext(), llvm::APInt(32, 0, true)),
          llvm::ConstantInt::get(
            llvm::getGlobalContext(), llvm::APInt(32, static_cast<size_t>(-n), true)));
    }
  }

  llvm::Value* const_uint(size_t n) {
#ifdef DEBUG
    if (n > (1 << 30)) {
      std::cerr << "FATAL: Potential overflow on compile-time integer constants" << std::endl;
    }
#endif

    // The safety of this cast is verified only in debug mode
    return llvm::ConstantInt::get(llvm::getGlobalContext(),
        llvm::APInt(32, static_cast<size_t>(n), false));
  }


  llvm::Value* const_real(double d) {
    return llvm::ConstantFP::get(llvm::getGlobalContext(),
        llvm::APFloat(d));
  }

  llvm::Value* const_false() {
    return llvm::ConstantInt::get(llvm::getGlobalContext(),
        llvm::APInt(1, 0, false));
  }


  llvm::Value* const_true() {
    return llvm::ConstantInt::get(llvm::getGlobalContext(),
        llvm::APInt(1, 1, false));
  }

  llvm::Value* const_bool(bool b) {
    return b ? const_true() : const_false();
  }

  llvm::ConstantInt* const_char(char c) {
    // TODO check safety of char cast
    return llvm::ConstantInt::get(llvm::getGlobalContext(),
        llvm::APInt(8, static_cast<size_t>(c), false));
  }

  llvm::Value* global_string(llvm::IRBuilder<>& bldr, const std::string& s) {
    auto iter = global_strings.find(s);
    if (iter != global_strings.end()) {
      return iter->second;
    }
    return global_strings[s] = bldr.CreateGlobalStringPtr(s);
  }
}  // namespace data

namespace builtin {
  llvm::Function* ascii() {
    static llvm::Function* ascii_ = nullptr;
    if (ascii_ != nullptr) return ascii_;

    ascii_ = llvm::Function::Create(
        Type::get_function(Type::get_uint(), Type::get_char())->llvm(),
        llvm::Function::ExternalLinkage, "ascii", global_module);

    llvm::Value* val = ascii_->args().begin();
    llvm::IRBuilder<> bldr(llvm::getGlobalContext());

    auto entry_block = llvm::BasicBlock::Create(
        llvm::getGlobalContext(), "entry", ascii_);

    bldr.SetInsertPoint(entry_block);
    // TODO check bounds if build option specified

    bldr.CreateRet(bldr.CreateTrunc(val, Type::get_char()->llvm()));

    return ascii_;
  }

}  // namespace builtin
