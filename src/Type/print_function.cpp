#include "Type.h"
#include "Scope.h"

extern llvm::Module* global_module;

namespace cstdlib {
  extern llvm::Constant* putchar();
  extern llvm::Constant* printf();

}  // namespace cstdlib

namespace data {
  extern llvm::Value* global_string(llvm::IRBuilder<>& bldr, const std::string& s);
}  // namespace data

void Primitive::call_print(llvm::IRBuilder<>& bldr, llvm::Value* val) {
  if (this == get_bool()) {
    call_repr(bldr, val);

  } else if (this == get_char()) {
    bldr.CreateCall(cstdlib::putchar(), { val });

  } else if (this == get_int()) {
    call_repr(bldr, val);

  } else if (this == get_real()) {
    call_repr(bldr, val);
    
  } else if (this == get_type()) {
    call_repr(bldr, val);

  } else if (this == get_uint()) {
    bldr.CreateCall(cstdlib::printf(),
        { data::global_string(bldr, "%u"), val });
  }
}


// TODO complete these
// void Pointer::call_print(llvm::IRBuilder<>& bldr, llvm::Value* val) {}
// void Tuple::call_print(llvm::IRBuilder<>& bldr, llvm::Value* val) {}

void UserDefined::call_print(llvm::IRBuilder<>& bldr, llvm::Value* val) {
  if (print_fn_ != nullptr) {
    bldr.CreateCall(print_fn_, { val });
  }
  // TODO ensure that you only call print if you have defined it.
}

void Primitive::set_print(llvm::Function* fn) {}
void Pointer::set_print(llvm::Function* fn) {}
void Tuple::set_print(llvm::Function* fn) {}
void Function::set_print(llvm::Function* fn) {}
void Array::set_print(llvm::Function* fn) {}
void UserDefined::set_print(llvm::Function* fn) {
  print_fn_ = fn;
  print_fn_->setName("print." + to_string());
}
