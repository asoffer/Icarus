#ifndef ICARUS_UNITY
#include "Type.h"
#endif

std::string Array::to_string() const {
  std::stringstream ss;
  ss << "[";
  if (fixed_length) {
    ss << len;
  } else {
    ss << "--";
  }
  Type *const *type_ptr_ptr = &data_type;

  while ((*type_ptr_ptr)->is_array()) {
    auto array_ptr = (const Array *)*type_ptr_ptr;
    ss << ", ";
    if (array_ptr->fixed_length) {
      ss << array_ptr->len;
    } else {
      ss << "--";
    }

    type_ptr_ptr = &array_ptr->data_type;
  }

  ss << "; " << **type_ptr_ptr << "]";
  return ss.str();
}

std::string Function::to_string() const {
  std::stringstream ss;
  if (input->is_function()) {
    ss << "(" << *input << ")";

  } else {
    ss << *input;
  }

  ss << " -> " << *output;
  return ss.str();
}

std::string Pointer::to_string() const {
  std::stringstream ss;
  if (pointee->is_function()) {
    ss << "&(" << *pointee << ")";
  } else {
    ss << "&" << *pointee;
  }
  return ss.str();
}

std::string Tuple::to_string() const {
  std::stringstream ss;

  auto iter = entries.begin();
  ss << "(" << **iter;
  ++iter;
  while (iter != entries.end()) {
    ss << ", " << **iter;
    ++iter;
  }
  ss << ")";

  return ss.str();
}

std::string TypeVariable::to_string() const { return identifier->token; }

std::string RangeType::to_string() const {
  return "Range(" + end_type->to_string() + ")";
}
std::string SliceType::to_string() const {
  return array_type->to_string() + "[..]";
}
