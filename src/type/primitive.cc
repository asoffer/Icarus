#include "type.h"

Primitive::Primitive(PrimType pt) : type_(pt) {}


std::string Primitive::to_string() const {
  switch (type_) {
  case PrimType::Err: return "Err";
  case PrimType::Unknown: return "???";
  case PrimType::Bool: return "bool";
  case PrimType::Char: return "char";
  case PrimType::Code: return "code";
  case PrimType::Int: return "int";
  case PrimType::Real: return "real";
  case PrimType::Type: return "type";
  case PrimType::Uint: return "uint";
  case PrimType::Void: return "void";
  case PrimType::NullPtr: return "null";
  case PrimType::String: return "string";
  default: UNREACHABLE;
  }
}
