#ifndef ICARUS_UNITY
#include "Type.h"
#endif

namespace cstdlib {
extern llvm::Constant *malloc();
extern llvm::Constant *free();
} // namespace cstdlib

// TODO make bytes() and alignment() platform specific
size_t Pointer::bytes() const { return 8; }
size_t Function::bytes() const { return 8; }
size_t TypeVariable::bytes() const { return 0; } // TODO should be uncallable

size_t Array::bytes() const {
  if (!fixed_length) { return 16; }
  auto size = MoveForwardToAlignment(data_type->bytes() * len, alignment());
  // TODO fix this hack that forces arrays to take of at least a byte so we
  // don't have overlapping structs. this is only an issue because we index
  // allocations by their stack location. we need to stop doing that.
  return size ? size : 1;
}

size_t Pointer::alignment() const { return 8; }
size_t Function::alignment() const { return 8; }
size_t TypeVariable::alignment() const { return 0; } // TODO should be uncallable

size_t Array::alignment() const {
  return fixed_length ? data_type->alignment() : 8;
}

size_t Tuple::bytes() const { NOT_YET; }
size_t Tuple::alignment() const { NOT_YET; }
size_t SliceType::bytes() const { NOT_YET; }
size_t SliceType::alignment() const { NOT_YET; }
size_t RangeType::bytes() const { NOT_YET; }
size_t RangeType::alignment() const { NOT_YET; }

Array::Array(Type *t)
    : init_func(nullptr), repr_func(nullptr), destroy_func(nullptr),
      data_type(t), len(0), fixed_length(false) {
  dimension =
      data_type->is_array() ? 1 + ((Array *)data_type)->dimension : 1;
  has_vars = data_type->has_vars;
}

Array::Array(Type *t, size_t l)
    : init_func(nullptr), repr_func(nullptr), destroy_func(nullptr),
      data_type(t), len(l), fixed_length(true) {
  dimension = data_type->is_array() ? 1 + ((Array *)data_type)->dimension : 1;
  has_vars = data_type->has_vars;
}

Tuple::Tuple(const std::vector<Type *> &entries) : entries(entries) {
  for (const auto &entry : entries) {
    if (has_vars) break;
    has_vars = entry->has_vars;
  }
}

Pointer::Pointer(Type *t) : pointee(t) { has_vars = pointee->has_vars; }

Function::Function(Type *in, Type *out) : input(in), output(out) {
  has_vars = input->has_vars || output->has_vars;
}

std::ostream &operator<<(std::ostream &os, const Type &t) {
  return os << t.to_string();
}

Type::operator llvm::Type *() const {
  generate_llvm();
  return llvm_type;
}

Function::operator llvm::FunctionType *() const {
  generate_llvm();
  return (llvm::FunctionType *)llvm_type;
}

bool Type::is_big() const { return is_array() || is_struct() || is_function(); }
bool Type::stores_data() const {
  return this != Type_ && !is_function() && !is_type_variable();
}

std::ostream &operator<<(std::ostream &os, const Type *&t) { return os << *t; }
