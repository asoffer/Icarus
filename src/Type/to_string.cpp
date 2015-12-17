#include "Type.h"

#include <string>
#include <sstream>

constexpr const char* type_strings[ Primitive::num_primitive_types_ ] = {
  "type_error",
  "??",
  "bool",
  "char",
  "int",
  "real",
  "type",
  "uint",
  "void"
};

std::string Primitive::to_string() const {
  return type_strings[prim_type_];
}

std::string Function::to_string() const {
  std::stringstream ss;
  bool needs_parens = argument_type()->is_function();
  if (needs_parens) ss << "(";
  ss << argument_type()->to_string();
  if (needs_parens) ss << ")";

  ss << " -> ";

  needs_parens = return_type()->is_function();
  if (needs_parens) ss << "(";
  ss << return_type()->to_string();
  if (needs_parens) ss << ")";
  return ss.str();
}

std::string Pointer::to_string() const {
  std::stringstream ss;
  ss << "&";
  bool needs_parens = pointee_type_->is_function();
  ss << (needs_parens ? "(" : "");
  ss << pointee_type_->to_string();
  ss << (needs_parens ? ")" : "");
  return ss.str();
}

std::string Tuple::to_string() const {
  std::stringstream ss;

  auto iter = entry_types_.begin();
  ss << "(" << (*iter)->to_string();
  ++iter;
  while (iter != entry_types_.end()) {
    ss << ", " << (*iter)->to_string();
    ++iter;
  }
  ss << ")";

  return ss.str();
}

std::string Array::to_string() const {
  std::stringstream ss;
  ss << "[-";

  const Type* type_ptr = type_;

  while (type_ptr->is_array()) {
    auto array_ptr = static_cast<const Array*>(type_ptr);
    ss << ", -";
    type_ptr = array_ptr->type_;
  }

  ss << "; " << type_ptr->to_string() << "]";
  return ss.str();
}


std::string UserDefined::to_string() const {
  // TODO 
  return "<TYPE>";
}
