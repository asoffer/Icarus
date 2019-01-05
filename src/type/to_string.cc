#include "type/all.h"

#include <cstring>

namespace type {
void Primitive::WriteTo(std::string *result) const {
  switch (type_) {
#define PRIMITIVE_MACRO(EnumName, name)                                        \
  case PrimType::EnumName:                                                     \
    result->append(name);                                                      \
    return;
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
    default: UNREACHABLE();
  }
}

void Array::WriteTo(std::string *result) const {
  result->append("[");
  result->append(std::to_string(len));
  Type const *t = data_type;
  while (auto *array_ptr = t->if_as<Array>()) {
    result->append(", ");
    result->append(std::to_string(array_ptr->len));
    t = array_ptr->data_type;
  }
  result->append("; ");
  data_type->WriteTo(result);
  result->append("]");
}

void Struct::WriteTo(std::string *result) const {
  result->append("struct.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

void GenericStruct::WriteTo(std::string *result) const {
  result->append("[");
  if (!deps_.empty()) {
    deps_[0]->WriteTo(result);
    for (size_t i = 1; i < deps_.size(); ++i) {
      result->append(", ");
      deps_[i]->WriteTo(result);
    }
  }
  result->append("; struct]");
}

void Enum::WriteTo(std::string *result) const {
  result->append("enum.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

void Flags::WriteTo(std::string *result) const {
  result->append("flags.");
  result->append(std::to_string(reinterpret_cast<uintptr_t>(this)));
}

void BufferPointer::WriteTo(std::string *result) const {
  bool needs_paren = pointee->is<Struct>() || pointee->is<Primitive>() ||
                     pointee->is<Enum>() || pointee->is<Flags>() ||
                     pointee->is<Array>() || pointee->is<Pointer>();
  result->append("[*]");
  if (needs_paren) { result->append("("); }
  pointee->WriteTo(result);
  if (needs_paren) { result->append(")"); }
}

void Pointer::WriteTo(std::string *result) const {
  bool needs_paren = pointee->is<Struct>() || pointee->is<Primitive>() ||
                     pointee->is<Enum>() || pointee->is<Flags>() ||
                     pointee->is<Array>() || pointee->is<Pointer>();
  result->append("*");
  if (needs_paren) { result->append("("); }
  pointee->WriteTo(result);
  if (needs_paren) { result->append(")"); }
}

void Function::WriteTo(std::string *result) const {
  if (input.empty()) {
    result->append("()");
  } else if (input.size() == 1 && !input[0]->is<Function>()) {
    input.at(0)->WriteTo(result);
  } else {
    result->append("(");
    input.at(0)->WriteTo(result);
    for (size_t i = 1; i < input.size(); ++i) {
      result->append(", ");
      input.at(i)->WriteTo(result);
    }
    result->append(")");
  }

  result->append(" -> ");

  if (output.empty()) {
    result->append("()");
  } else if (output.size() == 1) {
    output.at(0)->WriteTo(result);
  } else {
    result->append("(");
    output.at(0)->WriteTo(result);
    for (size_t i = 1; i < output.size(); ++i) {
      result->append(", ");
      output.at(i)->WriteTo(result);
    }
    result->append(")");
  }
}

void Variant::WriteTo(std::string *result) const {
  auto iter = variants_.begin();

  if ((*iter)->is<Struct>() || (*iter)->is<Primitive>() ||
      (*iter)->is<Enum>() || (*iter)->is<Flags>() || (*iter)->is<Pointer>() ||
      (*iter)->is<Function>() || (*iter)->is<Array>()) {
    (*iter)->WriteTo(result);
  } else {
    result->append("(");
    (*iter)->WriteTo(result);
    result->append(")");
  }

  ++iter;
  for (; iter != variants_.end(); ++iter) {
    result->append(" | ");
    if ((*iter)->is<Struct>() || (*iter)->is<Primitive>() ||
        (*iter)->is<Enum>() || (*iter)->is<Flags>() || (*iter)->is<Pointer>() ||
        (*iter)->is<Function>() || (*iter)->is<Array>()) {
      (*iter)->WriteTo(result);
    } else {
      result->append("(");
      (*iter)->WriteTo(result);
      result->append(")");
    }
  }
}

void Tuple::WriteTo(std::string *result) const {
  if (entries_.empty()) {
    result->append("()");
    return;
  }
  result->append("(");
  auto iter = entries_.begin();
  (*iter)->WriteTo(result);
  ++iter;
  for (; iter != entries_.end(); ++iter) {
    result->append(", ");
    (*iter)->WriteTo(result);
  }
  result->append(")");
}

void Opaque::WriteTo(std::string *result) const { result->append("<opaque>"); }

}  // namespace type
