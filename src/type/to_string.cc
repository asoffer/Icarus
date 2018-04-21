#include "all.h"

#include "../ast/ast.h"
#include <cstring>

namespace type {
size_t Primitive::string_size() const {
  switch (type_) {
#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  case PrimType::EnumName:                                                     \
    return sizeof(#name) - 1;
#include "primitive.xmacro.h"
#undef PRIMITIVE_MACRO
  default: UNREACHABLE();
  }
}

char *Primitive::WriteTo(char *buf) const {
  switch (type_) {
#define PRIMITIVE_MACRO(GlobalName, EnumName, name)                            \
  case PrimType::EnumName:                                                     \
    return std::strcpy(buf, #name) + string_size();
#include "primitive.xmacro.h"
#undef PRIMITIVE_MACRO
  default: UNREACHABLE();
  }
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

  const Type *const *type_ptr_ptr = &data_type;
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

  const Type *const *type_ptr_ptr = &data_type;
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

size_t Struct::string_size() const {
  size_t acc = 0;
  for (const auto &field : fields_) { acc += field.type->string_size(); }
  return acc + 2 * fields_.size() + 9;
}
char *Struct::WriteTo(char *buf) const {
  if (fields_.empty()) {
    buf = std::strcpy(buf, "struct {}") + 9;
    return buf;
  }
  buf = std::strcpy(buf, "struct { ") + 9;
  auto iter = fields_.begin();
  buf = iter->type->WriteTo(buf);
  ++iter;
  for (; iter != fields_.end(); ++iter) {
    buf = std::strcpy(buf, ", ") + 2;
    buf = iter->type->WriteTo(buf);
  }
  buf = std::strcpy(buf, " }") + 2;
  return buf;
}

size_t Enum::string_size() const { return bound_name.size(); }
char* Enum::WriteTo(char *buf) const {
  return std::strcpy(buf, bound_name.c_str()) + string_size();
}

size_t Pointer::string_size() const {
  return ((pointee->is<Struct>() || pointee->is<Primitive>() ||
           pointee->is<Enum>() || pointee->is<Array>() ||
           pointee->is<Pointer>())
              ? 1
              : 3) +
         pointee->string_size();
}
char* Pointer::WriteTo(char *buf) const {
  if (pointee->is<Struct>() || pointee->is<Primitive>() ||
      pointee->is<Enum>() || pointee->is<Array>() || pointee->is<Pointer>()) {
    buf = std::strcpy(buf, "*") + 1;
    buf = pointee->WriteTo(buf);
  } else {
    buf = std::strcpy(buf, "*(") + 2;
    buf = pointee->WriteTo(buf);
    buf = std::strcpy(buf, ")") + 1;
  }
  return buf;
}
size_t Function::string_size() const {
  size_t acc = 0;
  for (const Type *t : input) { acc += t->string_size(); }
  for (const Type *t : output) { acc += t->string_size(); }
  acc += 2 * (input.size() - 1) +       // space between inputs
         (input.size() == 1 ? 0 : 2) +  // Parens
         (input.empty() ? 4 : 0) +      // void
         4 +                            // " -> "
         2 * (output.size() - 1) +      // space between outputs
         (output.size() == 1 ? 0 : 2) + // Parens
         (output.empty() ? 4 : 0) +     // void
         (input.size() == 1 && input[0]->is<Function>() ? 2 : 0); // parens
  return acc;
}

char *Function::WriteTo(char *buf) const {
  if (input.empty()) {
    buf = std::strcpy(buf, "void") + 4;
  } else if (input.size() == 1 && !input[0]->is<Function>()) {
    buf = input[0]->WriteTo(buf);
  } else {
    buf = std::strcpy(buf, "(") + 1;
    buf = input[0]->WriteTo(buf);
    for (size_t i = 1; i < input.size(); ++i) {
      buf = std::strcpy(buf, ", ") + 2;
      buf = input[i]->WriteTo(buf);
    }
    buf = std::strcpy(buf, ")") + 1;
  }

  buf = std::strcpy(buf, " -> ") + 4;

  if (output.empty()) {
    buf = std::strcpy(buf, "void") + 4;
  } else if (output.size() == 1) {
    buf = output[0]->WriteTo(buf);
  } else {
    buf = std::strcpy(buf, "(") + 1;
    buf = output[0]->WriteTo(buf);
    for (size_t i = 1; i < output.size(); ++i) {
      buf = std::strcpy(buf, ", ") + 2;
      buf = output[i]->WriteTo(buf);
    }
    buf = std::strcpy(buf, ")") + 1;
  }

  return buf;
}

size_t Variant::string_size() const {
  size_t result = (variants_.size() - 1) * 3;
  for (const Type *v : variants_) {
    result +=
        v->string_size() + (v->is<Struct>() || v->is<Primitive>() ||
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

size_t Range::string_size() const { return 7 + end_type->string_size(); }
char *Range::WriteTo(char *buf) const {
  buf = std::strcpy(buf, "Range(") + 6;
  buf = end_type->WriteTo(buf);
  buf = std::strcpy(buf, ")") + 1;
  return buf;
}

size_t Slice::string_size() const { return 4 + array_type->string_size(); }
char *Slice::WriteTo(char *buf) const {
  buf = array_type->WriteTo(buf);
  buf = std::strcpy(buf, "[..]") + 4;
  return buf;
}

size_t Scope::string_size() const {
  size_t result = 5 + 2 * types_.size();
  for (const Type *t : types_) { result += t->string_size(); }
  return result;
}

char *Scope::WriteTo(char *buf) const {
  buf = std::strcpy(buf, "scope(") + 6;
  auto iter = types_.begin();
  buf = (*iter)->WriteTo(buf);
  ++iter;
  for (; iter != types_.end(); ++iter) {
    buf = std::strcpy(buf, ", ") + 2;
    buf = (*iter)->WriteTo(buf);
  }
  buf = std::strcpy(buf, ")") + 1;
  return buf;
}

size_t Tuple::string_size() const {
  size_t result = 2 * entries_.size();
  for (const Type *t : entries_) { result += t->string_size(); }
  return result;
}

char *Tuple::WriteTo(char *buf) const {
  buf = std::strcpy(buf, "(") + 1;
  auto iter = entries_.begin();
  buf = (*iter)->WriteTo(buf);
  ++iter;
  for (; iter != entries_.end(); ++iter) {
    buf = std::strcpy(buf, ", ") + 2;
    buf = (*iter)->WriteTo(buf);
  }
  buf = std::strcpy(buf, ")") + 1;
  return buf;
}
} // namespace type
