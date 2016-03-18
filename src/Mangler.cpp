#include <string>
#include <sstream>

#include "Type.h"

std::string Mangle(const Type *t, bool prefix) {
  if (t->is_primitive()) {
    if (t == Bool) {
      return "b";
    } else if (t == Char) {
      return "c";
    } else if (t == Int) {
      return "i";
    } else if (t == Real) {
      return "r";
    } else if (t == Uint) {
      return "u";
    } else if (t == Void) {
      return "v";
    } else {
      assert(false && "Invalid type name to be mangled");
    }
  }

  std::stringstream ss;
  if (prefix) ss << "_Z";

  if (t->is_array()) {
    ss << "A0" << Mangle(static_cast<const Array*>(t)->data_type.get, false);
  } else if (t->is_pointer()) {
    ss << "P" << Mangle(static_cast<const Pointer*>(t)->pointee.get, false);
  } else {
    ss << t->to_string();
  }

  return ss.str();
}
