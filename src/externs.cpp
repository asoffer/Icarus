#include "AST.h"
#include "ErrorLog.h"
#include "typedefs.h"

#include <map>
#include <queue>

// TODO 32 is hard-coded here as an int size. Change it

// Debug flags and their default values
namespace debug {
  // Turns on step-by-step iteration through the shifting and reducing.
  bool parser = false;

  // Turns on debug printing in the dependency system
  bool dependency_system = false;
}

std::map<std::string, StmtsPtr> ast_map;

enum class Lib {
  String
};

std::map<Lib, IdPtr> lib_type;
std::queue<std::string> file_queue;

llvm::Module* global_module;
llvm::DataLayout* data_layout;

std::map<std::string, llvm::Value*> global_strings;

ErrorLog error_log;

llvm::BasicBlock* make_block(const std::string& name, llvm::Function* fn) {
  return llvm::BasicBlock::Create(llvm::getGlobalContext(), name, fn);
}

#define CSTDLIB(fn, variadic, in, out)        \
  llvm::Constant* fn() {                      \
    static llvm::Constant* func_ =            \
    global_module->getOrInsertFunction(#fn,   \
        llvm::FunctionType::get(out->llvm(),  \
          { in->llvm() }, variadic));         \
    return func_;                             \
  }



// TODO Reduce the dependency on the C standard library. This probably means
// writing platform-specific assembly.
namespace cstdlib {
  CSTDLIB(free,    false, Type::get_pointer(Type::get_char()), Type::get_void());
  CSTDLIB(calloc,  false, Type::get_uint(), Type::get_pointer(Type::get_char()));
  CSTDLIB(malloc,  false, Type::get_uint(), Type::get_pointer(Type::get_char()));
  //CSTDLIB(memcpy,  false, Type::get_tuple({ Type::get_pointer(Type::get_char()), Type::get_pointer(Type::get_char()), Type::get_uint() }), Type::get_pointer(Type::get_char()));
  CSTDLIB(putchar, false, Type::get_char(), Type::get_int());
  CSTDLIB(puts,    false, Type::get_pointer(Type::get_char()), Type::get_int());
  CSTDLIB(printf,  true,  Type::get_pointer(Type::get_char()), Type::get_int());

  llvm::Constant* memcpy() {                     
    static llvm::Constant* func_ =            
      global_module->getOrInsertFunction("memcpy",   
          llvm::FunctionType::get(Type::get_pointer(Type::get_char())->llvm(),
            {
            Type::get_pointer(Type::get_char())->llvm(),
            Type::get_pointer(Type::get_char())->llvm(),
            Type::get_uint()->llvm()
            }, false));
    return func_;                             
  }



}  // namespace cstdlib

namespace data {
  llvm::Value* null_pointer(Type* t) {
    return llvm::ConstantPointerNull::get(llvm::PointerType::get(t->llvm(), 0));
  }

  llvm::Value* const_int(int n) {
    return llvm::ConstantInt::get(llvm::getGlobalContext(),
        llvm::APInt(32, static_cast<unsigned int>(n), false));
  }

  llvm::Value* const_neg(llvm::IRBuilder<>& bldr, size_t n) {
#ifdef DEBUG
    if (n > (1 << 30)) {
      std::cerr << "FATAL: Potential overflow on compile-time integer constants" << std::endl;
    }
#endif

    return bldr.CreateSub(
       llvm::ConstantInt::get(
         llvm::getGlobalContext(), llvm::APInt(32, 0, true)),
       llvm::ConstantInt::get(
         llvm::getGlobalContext(), llvm::APInt(32, n, true)));
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

    auto entry_block = make_block("entry", ascii_);

    bldr.SetInsertPoint(entry_block);
    // TODO check bounds if build option specified

    bldr.CreateRet(bldr.CreateTrunc(val, Type::get_char()->llvm()));

    return ascii_;
  }
}  // namespace builtin

#undef CSTDLIB
