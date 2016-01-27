#include "Type.h"
extern llvm::DataLayout* data_layout;

constexpr size_t pointer_size_in_bytes = sizeof(void*);

size_t Function::bytes() const { return pointer_size_in_bytes; }

size_t Pointer::bytes() const { return pointer_size_in_bytes; }

size_t TypeSystem::Primitive::bytes() const {
  return (llvm_type_ == nullptr)
    ? 0 : data_layout->getTypeStoreSize(llvm_type_);
}

size_t Enum::bytes() const { return Uint->bytes(); }

size_t Array::bytes() const { return pointer_size_in_bytes; }  // TODO

size_t Tuple::bytes() const {
  // TODO add in padding
  size_t output = 0;
  for (auto ty : entry_types_) {
    output += ty->bytes();
  }

  return output;
}
size_t UserDefined::bytes() const {
  // TODO add in padding
  return 0;
}

