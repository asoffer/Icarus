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

void Primitive::call_print(llvm::Value* val) {
  if (this == Char) {
    builder.CreateCall(cstdlib::putchar(), { val });

  } else if (this == Uint) {
    builder.CreateCall(cstdlib::printf(),
                       {data::global_string(builder, "%u"), val});

  } else {
    call_repr(val);
  }
}


// TODO complete these
// void Pointer::call_print(llvm::Value* val) {}
// void Tuple::call_print(llvm::Value* val) {}

void Structure::call_print(llvm::Value* val) {
  if (print_fn_ != nullptr) {
    builder.CreateCall(print_fn_, { val });
  }
  // TODO ensure that you only call print if you have defined it.
}

void Structure::set_print(llvm::Function* fn) {
  print_fn_ = fn;
  print_fn_->setName("print." + Mangle(this));
}
