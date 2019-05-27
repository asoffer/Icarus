#ifndef ICARUS_CORE_BUILTIN_H
#define ICARUS_CORE_BUILTIN_H

namespace core {
enum class Builtin : char {
#define ICARUS_CORE_BUILTIN_X(enumerator, ...) enumerator,
#include "core/builtin.xmacro.h"
#undef ICARUS_CORE_BUILTIN_X
};
}  // namespace core

#endif  // ICARUS_CORE_BUILTIN_H
