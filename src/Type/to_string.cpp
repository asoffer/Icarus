#include "Type.h"

#include <string>
#include <sstream>

std::string Primitive::to_string() const {
  switch (type_) {
    case TypeEnum::Error:   return "!!!";
    case TypeEnum::Unknown: return "???";
    case TypeEnum::Bool:    return "bool";
    case TypeEnum::Char:    return "char";
    case TypeEnum::Int:     return "int";
    case TypeEnum::Real:    return "real";
    case TypeEnum::Type:    return "type";
    case TypeEnum::Uint:    return "uint";
    case TypeEnum::Void:    return "void";
    case TypeEnum::NullPtr: return "null";
  }
}

std::string Array::to_string() const {
  std::stringstream ss;
  ss << "[-";
  const TypePtr *type_ptr_ptr = &data_type;

  while (type_ptr_ptr->is_array()) {
    ss << ", -";
    type_ptr_ptr = &static_cast<const Array*>(type_ptr_ptr->get)->data_type;
  }

  ss << "; " << *type_ptr_ptr << "]";
  return ss.str();
}

std::string Function::to_string() const {
  std::stringstream ss;
  if (input.is_function()) {
    ss << "(" << input << ")";

  } else {
    ss << input;
  }

  ss << " -> ";

  if (output.is_function()) {
    ss << "(" << output << ")";

  } else {
    ss << output;
  }
  return ss.str();
}

std::string Pointer::to_string() const {
  std::stringstream ss;
  if (pointee.is_function()) {
    ss << "&(" << pointee << ")";
  } else {
    ss << "&" << pointee;
  }
  return ss.str();
}

std::string Tuple::to_string() const {
  std::stringstream ss;

  auto iter = entries.begin();
  ss << "(" << *iter;
  ++iter;
  while (iter != entries.end()) {
    ss << ", " << *iter;
    ++iter;
  }
  ss << ")";

  return ss.str();
}

std::string DependentType::to_string() const {
  return "(DependentType)";
}

std::string TypeVariable::to_string() const {
  return "Var(" + identifier->token() + ")";
}

std::string ForwardDeclaration::to_string() const {
  return "ForwardDeclaration(" + std::to_string(index) + ")";
}


std::string Structure::to_string() const { return bound_name; }
std::string Enumeration::to_string() const { return bound_name; }
