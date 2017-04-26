#include "type.h"
#include "../scope.h"

// TODO make bytes() and alignment() platform specific
size_t Pointer::bytes() const { return 8; }
size_t Function::bytes() const { return 8; }
size_t Scope_Type::bytes() const { return 8; }
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
size_t Scope_Type::alignment() const { return 8; }
size_t Array::alignment() const {
  return fixed_length ? data_type->alignment() : 8;
}

#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  bool Primitive::is_##name() const { return type_ == PrimType::EnumName; }
#include "../config/primitive.conf"
#undef PRIMITIVE_MACRO


size_t Tuple::bytes() const { NOT_YET; }
size_t Tuple::alignment() const { NOT_YET; }
size_t SliceType::bytes() const { NOT_YET; }
size_t SliceType::alignment() const { NOT_YET; }
size_t RangeType::bytes() const { NOT_YET; }
size_t RangeType::alignment() const { NOT_YET; }

Array::Array(Type *t)
    : init_func(nullptr), repr_func(nullptr), destroy_func(nullptr),
      data_type(t), len(0), fixed_length(false) {
  dimension = data_type->is_array() ? 1 + ((Array *)data_type)->dimension : 1;
}

bool Array::private_has_vars() { return data_type->has_vars(); }
bool Scope_Type::private_has_vars() { return false; }

Array::Array(Type *t, size_t l)
    : init_func(nullptr), repr_func(nullptr), destroy_func(nullptr),
      data_type(t), len(l), fixed_length(true) {
  dimension = data_type->is_array() ? 1 + ((Array *)data_type)->dimension : 1;
}

Tuple::Tuple(const std::vector<Type *> &entries) : entries(entries) {}

bool Tuple::private_has_vars() {
  for (const auto &entry : entries) {
    if (entry->has_vars()) { return true; }
  }
  return false;
}

Pointer::Pointer(Type *t) : pointee(t) {}
bool Pointer::private_has_vars() { return pointee->has_vars(); }
bool SliceType::private_has_vars() { return array_type->has_vars(); }
bool RangeType::private_has_vars() { return end_type->has_vars(); }
bool TypeVariable::private_has_vars() { return true; }

Function::Function(Type *in, Type *out) : input(in), output(out) {}

bool Function::private_has_vars() { return input->has_vars() || output->has_vars(); }

std::ostream &operator<<(std::ostream &os, const Type &t) {
  return os << t.to_string();
}

bool Type::is_big() const { return is_array() || is_struct(); }
bool Type::stores_data() const {
  return this != Type_ && !is_function() && !is_type_variable();
}

std::ostream &operator<<(std::ostream &os, const Type *&t) { return os << *t; }
