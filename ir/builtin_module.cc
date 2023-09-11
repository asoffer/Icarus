#include "ir/builtin_module.h"

namespace ic {

Module BuiltinModule() {
  Module m;
  m.Insert(0, {.type = type::Bool, .value = {jasmin::Value(true)}});
  return m;
}

}  // namespace ic
