#ifndef ICARUS_UNITY
#include "Type.h"
#endif

namespace data {
extern llvm::ConstantInt *const_false();
extern llvm::ConstantInt *const_char(char c);
extern llvm::ConstantInt *const_int(long n);
extern llvm::ConstantFP *const_real(double d);
extern llvm::ConstantInt *const_uint(size_t n);
extern llvm::Constant *null(const Type *t);
} // namespace data

llvm::Constant *Primitive::InitialValue() const {
  if (this == Bool) {
    return data::const_false();
  } else if (this == Char) {
    return data::const_char('\0');
  } else if (this == Int) {
    return data::const_int(0);
  } else if (this == Real) {
    return data::const_real(0);
  } else if (this == Uint) {
    return data::const_uint(0);
  } else {
    assert(false && "Unknown initialization");
  }
}
llvm::Constant *Array::InitialValue() const {
  if (fixed_length) {
    auto init_elem = data_type->InitialValue();

    return llvm::ConstantArray::get(
        static_cast<llvm::ArrayType *>(llvm_type),
        std::vector<llvm::Constant *>(len, init_elem));

  } else {
    assert(false && "Unknown initialization");
  }
}

llvm::Constant *Pointer::InitialValue() const { return data::null(this); }

llvm::Constant *Tuple::InitialValue() const { assert(false); }
llvm::Constant *Enumeration::InitialValue() const { assert(false); }
llvm::Constant *Function::InitialValue() const { assert(false); }
llvm::Constant *Structure::InitialValue() const { assert(false); }
llvm::Constant *TypeVariable::InitialValue() const { assert(false); }
llvm::Constant *ParametricStructure::InitialValue() const { assert(false); }
llvm::Constant *QuantumType::InitialValue() const { assert(false); }
llvm::Constant *RangeType::InitialValue() const { assert(false); }
llvm::Constant *SliceType::InitialValue() const { assert(false); }
