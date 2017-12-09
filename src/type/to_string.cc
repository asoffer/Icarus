#include "type.h"

#include "../ast/ast.h"
#include <cstring>

size_t Primitive::string_size() const {
  switch (type_) {
#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  case PrimType::EnumName:                                                     \
    return sizeof(#name) / sizeof((#name[0])) - 1;
#include "../config/primitive.conf"
#undef PRIMITIVE_MACRO
  default: UNREACHABLE();
  }
}

char *Primitive::WriteTo(char *buf) const {
  switch (type_) {
#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  case PrimType::EnumName:                                                     \
    return std::strcpy(buf, #name) + string_size();
#include "../config/primitive.conf"
#undef PRIMITIVE_MACRO
  default: UNREACHABLE();
  }
}

size_t Struct::string_size() const { return bound_name.size(); }
char* Struct::WriteTo(char *buf) const {
  return std::strcpy(buf, bound_name.c_str()) + string_size();
}

size_t Enum::string_size() const { return bound_name.size(); }
char* Enum::WriteTo(char *buf) const {
  return std::strcpy(buf, bound_name.c_str()) + string_size();
}

size_t Pointer::string_size() const {
  return ((pointee->is<Struct>() || pointee->is<Primitive>() ||
           pointee->is<Enum>() || pointee->is<Array>())
              ? 1
              : 3) +
         pointee->string_size();
}
char* Pointer::WriteTo(char *buf) const {
  if (pointee->is<Struct>() || pointee->is<Primitive>() ||
      pointee->is<Enum>() || pointee->is<Array>()) {
    buf = std::strcpy(buf, "*") + 1;
    buf = pointee->WriteTo(buf);
  } else {
    buf = std::strcpy(buf, "*(") + 2;
    buf = pointee->WriteTo(buf);
    buf = std::strcpy(buf, ")") + 1;
  }
  return buf;
}
static size_t NumDigits(size_t num) {
  size_t num_digits = 1;
  while (num >= 10) {
    num /= 10;
    num_digits++;
  }
  return num_digits;
}

size_t Array::string_size() const {
  size_t result = 1 + (fixed_length ? NumDigits(len) : 2);

  Type *const *type_ptr_ptr = &data_type;
  while ((*type_ptr_ptr)->is<Array>()) {
    auto array_ptr = &(*type_ptr_ptr)->as<const Array>();
    result += 2 + ((array_ptr->fixed_length) ? NumDigits(array_ptr->len) : 2);
    type_ptr_ptr = &array_ptr->data_type;
  }
  return result + 3 + (*type_ptr_ptr)->string_size();
}

char *Array::WriteTo(char *buf) const {
  buf = std::strcpy(buf, "[") + 1;
  if (fixed_length) {
    buf = std::strcpy(buf, std::to_string(len).c_str()) + NumDigits(len);
  } else {
    buf = std::strcpy(buf, "--") + 2;
  }

  Type *const *type_ptr_ptr = &data_type;
  while ((*type_ptr_ptr)->is<Array>()) {
    auto array_ptr = &(*type_ptr_ptr)->as<const Array>();

    if (array_ptr->fixed_length) {
      buf = std::strcpy(buf, ", ") + 2;
      buf = std::strcpy(buf, std::to_string(array_ptr->len).c_str()) +
            NumDigits(array_ptr->len);
    } else {
      buf = std::strcpy(buf, ", --") + 4;
    }
    type_ptr_ptr = &array_ptr->data_type;
  }
  buf = std::strcpy(buf, "; ") + 2;
  buf = (*type_ptr_ptr)->WriteTo(buf);
  buf = std::strcpy(buf, "]") + 1;
  return buf;
}

size_t Function::string_size() const {
  return input->string_size() +
         (input->is<Struct>() || input->is<Primitive>() || input->is<Enum>() ||
                  input->is<Pointer>() || input->is<Array>()
              ? 0
              : 2) +
         4 + output->string_size() +
         (output->is<Struct>() || output->is<Primitive>() ||
                  output->is<Enum>() || output->is<Pointer>() ||
                  output->is<Array>() || output->is<Function>()
              ? 0
              : 2);
}

char *Function::WriteTo(char *buf) const {
  if (input->is<Struct>() || input->is<Primitive>() || input->is<Enum>() ||
      input->is<Pointer>() || input->is<Array>()) {
    buf = input->WriteTo(buf);
    buf = std::strcpy(buf, " -> ") + 4;
  } else {
    buf = std::strcpy(buf, "(") + 1;
    buf = input->WriteTo(buf);
    buf = std::strcpy(buf, ") -> ") + 5;
  }
  if (output->is<Struct>() || output->is<Primitive>() || output->is<Enum>() ||
      output->is<Pointer>() || output->is<Array>() || output->is<Function>()) {
    buf = output->WriteTo(buf);
  } else {
    buf = std::strcpy(buf, "(") + 1;
    buf = output->WriteTo(buf);
    buf = std::strcpy(buf, ")") + 1;
  }
  return buf;
}

size_t Variant::string_size() const {
  size_t result = (variants_.size() - 1) * 3;
  for (Type *v : variants_) {
    result += v->string_size() + (v->is<Struct>() || v->is<Primitive>() ||
                                          v->is<Enum>() || v->is<Pointer>() ||
                                          v->is<Function>() || v->is<Array>()
                                      ? 0
                                      : 2);
  }
  return result;
}

char *Variant::WriteTo(char *buf) const {
  auto iter = variants_.begin();

  if ((*iter)->is<Struct>() || (*iter)->is<Primitive>() ||
      (*iter)->is<Enum>() || (*iter)->is<Pointer>() ||
      (*iter)->is<Function>() || (*iter)->is<Array>()) {
    buf = (*iter)->WriteTo(buf);
  } else {
    buf = std::strcpy(buf, "(") + 1;
    buf = (*iter)->WriteTo(buf);
    buf = std::strcpy(buf, ")") + 1;
  }

  ++iter;
  for (; iter != variants_.end(); ++iter) {
    buf = std::strcpy(buf, " | ") + 3;
    if ((*iter)->is<Struct>() || (*iter)->is<Primitive>() ||
        (*iter)->is<Enum>() || (*iter)->is<Pointer>() ||
        (*iter)->is<Function>() || (*iter)->is<Array>()) {
      buf = (*iter)->WriteTo(buf);
    } else {
      buf = std::strcpy(buf, "(") + 1;
      buf = (*iter)->WriteTo(buf);
      buf = std::strcpy(buf, ")") + 1;
    }
  }
  return buf;
}

size_t Tuple::string_size() const {
  size_t result = 2 * entries.size();
  for (Type *entry : entries) { result += entry->string_size(); }
  return result;
}
char *Tuple::WriteTo(char *buf) const {
  buf = std::strcpy(buf, "(") + 1;
  auto iter = entries.begin();
  buf = (*iter)->WriteTo(buf);
  ++iter;
  for (; iter != entries.end(); ++iter) {
    buf = std::strcpy(buf, ", ") + 2;
    buf = (*iter)->WriteTo(buf);
  }
  buf = std::strcpy(buf, ")") + 1;
  return buf;
}

size_t RangeType::string_size() const { return 7 + end_type->string_size(); }
char *RangeType::WriteTo(char *buf) const {
  buf = std::strcpy(buf, "Range(") + 6;
  buf = end_type->WriteTo(buf);
  buf = std::strcpy(buf, ")") + 1;
  return buf;
}

size_t SliceType::string_size() const { return 4 + array_type->string_size(); }
char *SliceType::WriteTo(char *buf) const {
  buf = array_type->WriteTo(buf);
  buf = std::strcpy(buf, "[..]") + 4;
  return buf;
}

size_t Scope_Type::string_size() const { return 7 + type_->string_size(); }
char *Scope_Type::WriteTo(char *buf) const {
  buf = std::strcpy(buf, "Scope(") + 6;
  buf = type_->WriteTo(buf);
  buf = std::strcpy(buf, ")") + 1;
  return buf;
}
