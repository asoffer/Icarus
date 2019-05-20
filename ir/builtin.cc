#include "ir/builtin.h"

#include "type/function.h"
#include "type/type.h"

namespace ir {
type::Type const *BuiltinType(Builtin b) {
  switch (b) {
#define IR_BUILTIN_MACRO(enumerator, str, t)                                   \
  case Builtin::enumerator:                                                    \
    return t;
#include "ir/builtin.xmacro.h"
#undef IR_BUILTIN_MACRO
  }
  UNREACHABLE();
}

std::string stringify(Builtin b) {
  switch (b) {
#define IR_BUILTIN_MACRO(enumerator, str, ...)                                 \
  case Builtin::enumerator:                                                    \
    return str;
#include "ir/builtin.xmacro.h"
#undef IR_BUILTIN_MACRO
  }
  UNREACHABLE();
}

}  // namespace ir
