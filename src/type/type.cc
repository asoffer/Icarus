#include "type.h"
#include "../scope.h"

#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  bool Primitive::is_##name() const { return type_ == PrimType::EnumName; }
#include "../config/primitive.conf"
#undef PRIMITIVE_MACRO

Array::Array(Type *t) : data_type(t), len(0), fixed_length(false) {
  dimension = data_type->is_array() ? 1 + ((Array *)data_type)->dimension : 1;
}

Array::Array(Type *t, size_t l) : data_type(t), len(l), fixed_length(true) {
  dimension = data_type->is_array() ? 1 + ((Array *)data_type)->dimension : 1;
}

Tuple::Tuple(const std::vector<Type *> &entries) : entries(entries) {}

Pointer::Pointer(Type *t) : pointee(t) {}

Function::Function(Type *in, Type *out) : input(in), output(out) {}

std::ostream &operator<<(std::ostream &os, const Type &t) {
  return os << t.to_string();
}

bool Type::is_big() const { return is_array() || is_struct(); }
bool Type::stores_data() const { return this != Type_ && !is_function(); }

std::ostream &operator<<(std::ostream &os, const Type *&t) { return os << *t; }
