#include "type/type.h"
#include "ast/ast.h"

struct Scope;

std::string Mangle(const Type *t, bool prefix) {
  if (t->is<Primitive>()) {
    if (t == Bool) { return "b"; }
    if (t == Char) { return "c"; }
    if (t == Int) { return "i"; }
    if (t == Real) { return "r"; }
    if (t == Uint) { return "u"; }
    if (t == Void) { return "v"; }
    if (t == Type_) { return "t"; }
    if (t == String) { return "s"; }
    UNREACHABLE(*t);
  }

  std::stringstream ss;
  if (prefix) ss << "_Z";

  if (t->is<Array>()) {
    auto array_type = (const Array *)t;
    ss << 'A' << (array_type->fixed_length ? array_type->len : 0)
       << Mangle(array_type->data_type, false);

  } else if (t->is<Pointer>()) {
    ss << "P" << Mangle(((const Pointer *)t)->pointee, false);

  } else if (t->is<Struct>()) {
    auto struct_type = (const Struct *)t;
    ss << "S" << struct_type->bound_name.size() << struct_type->bound_name;

  } else if (t->is<Function>()) {
    // TODO treat as function pointer?
    ss << "F" << t->as<Function>().input.size();
    for (Type *in : t->as<Function>().input) { ss << Mangle(in, false); }
  } else {
    ss << t->to_string();
  }

  return ss.str();
}
