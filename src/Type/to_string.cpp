#include "Type.h"

#include <string>
#include <sstream>

std::string TypeSystem::Primitive::to_string() const {
  switch (type_) {
    case Primitive::TypeEnum::Error:   return "!!!";
    case Primitive::TypeEnum::Unknown: return "???";
    case Primitive::TypeEnum::Bool:    return "bool";
    case Primitive::TypeEnum::Char:    return "char";
    case Primitive::TypeEnum::Int:     return "int";
    case Primitive::TypeEnum::Real:    return "real";
    case Primitive::TypeEnum::Type:    return "type";
    case Primitive::TypeEnum::Uint:    return "uint";
    case Primitive::TypeEnum::Void:    return "void";
  }
}

std::string Function::to_string() const {
  std::stringstream ss;
  bool needs_parens = argument_type()->is_function();
  if (needs_parens) ss << "(" << *argument_type() << ") -> ";
  else              ss << *argument_type() << " -> ";

  needs_parens = return_type()->is_function();
  if (needs_parens) ss << "(" << *return_type() << ")";
  else              ss << *return_type();

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
  ss << "(" << (*iter);
  ++iter;
  while (iter != entry_types_.end()) {
    ss << ", " << (*iter);
    ++iter;
  }
  ss << ")";

  return ss.str();
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


std::string UserDefined::to_string() const {
  // Hacky reverse lookup.
  // TODO Is it worth storing the table in a reverse-lookup as well?
  for (const auto& kv : lookup_) {
    if (kv.second == this) {
      return kv.first;
    }
  }

  assert(false && "User-defined type is not in the type database.");
}

std::string Enum::to_string() const {
  // Hacky reverse lookup.
  // TODO Is it worth storing the table in a reverse-lookup as well?
  for (const auto& kv : lookup_) {
    if (kv.second == this) {
      return kv.first;
    }
  }

  assert(false && "Enum is not in the type database.");
}
