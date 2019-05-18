#ifndef ICARUS_IR_BUILTIN_H
#define ICARUS_IR_BUILTIN_H

#include <string>

#include "base/debug.h"

namespace type {
struct Type;
}  // namespace type

namespace ir {
struct AnyFunc;

enum class Builtin : char {
#define IR_BUILTIN_MACRO(enumerator, ...) enumerator,
#include "ir/builtin.xmacro.h"
#undef IR_BUILTIN_MACRO
};

inline std::string stringify(Builtin b) {
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

#endif // ICARUS_IR_BUILTIN_H
