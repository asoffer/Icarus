#include "Type.h"

constexpr size_t pointer_size_in_bytes = sizeof(void*);
constexpr const size_t type_bytes[ Primitive::num_primitive_types_ ] = {
  0, 0, 1, 1, 4, 8, 0, 4, 0
};

size_t Function::bytes() const { return pointer_size_in_bytes; }

size_t Pointer::bytes() const { return pointer_size_in_bytes; }

size_t Primitive::bytes() const { return type_bytes[prim_type_]; }

size_t Enum::bytes() const { return Primitive::get_uint()->bytes(); }

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

