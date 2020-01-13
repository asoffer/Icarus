#ifndef ICARUS_TYPE_BASIC_TYPE_H
#define ICARUS_TYPE_BASIC_TYPE_H

#include <cstdint>

#include "base/debug.h"

namespace type {
struct Type;

enum class BasicType : uint8_t {
#define PRIMITIVE_MACRO(EnumName, name) EnumName,
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
};

inline char const *ToString(BasicType t) {
  switch (t) {
#define PRIMITIVE_MACRO(EnumName, name)                                        \
  case BasicType::EnumName:                                                    \
    return name;
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
  }
  UNREACHABLE();
}

Type const *Prim(BasicType b);

}  // namespace type

#endif  // ICARUS_TYPE_BASIC_TYPE_H
