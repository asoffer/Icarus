#ifndef ICARUS_TYPE_BASIC_TYPE_H
#define ICARUS_TYPE_BASIC_TYPE_H

#include <cstdint>

namespace type {
struct Type;

enum class BasicType : uint8_t {
#define PRIMITIVE_MACRO(EnumName, name) EnumName,
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
};

Type const *Prim(BasicType b);

}  // namespace type

#endif  // ICARUS_TYPE_BASIC_TYPE_H
