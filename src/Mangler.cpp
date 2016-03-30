#include <string>
#include <sstream>

#include "Type.h"

std::string Mangle(const Type *t, bool prefix) {
  if (t->is_primitive()) {
    if (t == Bool.get) {
      return "b";
    } else if (t == Char.get) {
      return "c";
    } else if (t == Int.get) {
      return "i";
    } else if (t == Real.get) {
      return "r";
    } else if (t == Uint.get) {
      return "u";
    } else if (t == Void.get) {
      return "v";
    } else {
      assert(false && "Invalid type name to be mangled");
    }
  }

  std::stringstream ss;
  if (prefix) ss << "_Z";

  if (t->is_array()) {
    ss << "A0" << Mangle(static_cast<const Array *>(t)->data_type.get, false);

  } else if (t->is_pointer()) {
    ss << "P" << Mangle(static_cast<const Pointer *>(t)->pointee.get, false);

  } else if (t->is_struct()) {
    auto struct_type = static_cast<const Structure *>(t);
    ss << "S" << struct_type->bound_name.size() << struct_type->bound_name;

  } else if (t->is_function()) {
    // TODO treat as function pointer?
    ss << "F" << Mangle(static_cast<const Function *>(t)->input.get, false);

  } else {
    ss << t->to_string();
  }

  return ss.str();
}

std::string Mangle(const Function *f, const std::string &name) {
  if (name == "main") return "main";

  std::stringstream ss;
  ss << "_ZF" << name.size() << name;
  ss << Mangle(f->input.get, false);
  return ss.str();
}
