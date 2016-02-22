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
  const Type* type_ptr = data_type();

  while (type_ptr->is_array()) {
    ss << ", -";
    type_ptr = static_cast<const Array*>(type_ptr)->data_type();
  }

  ss << "; " << *type_ptr << "]";
  return ss.str();
}

std::string Function::to_string() const {
  std::stringstream ss;
  if (argument_type()->is_function()) {
    ss << "(" << *argument_type() << ")";

  } else {
    ss << *argument_type();
  }

  ss << " -> ";

  if (return_type()->is_function()) {
    ss << "(" << *return_type() << ")";

  } else {
    ss << *return_type();
  }
  return ss.str();
}

std::string Pointer::to_string() const {
  std::stringstream ss;
  if (pointee_type()->is_function()) {
    ss << "&(" << *pointee_type() << ")";
  } else {
    ss << "&" << *pointee_type();
  }
  return ss.str();
}

std::string Tuple::to_string() const {
  std::stringstream ss;

  auto iter = entry_types_.begin();
  ss << "(" << *(*iter);
  ++iter;
  while (iter != entry_types_.end()) {
    ss << ", " << *(*iter);
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

std::string Structure::to_string() const { return bound_name; }
std::string Enumeration::to_string() const { return bound_name; }
