#ifndef ICARUS_COMMON_CONSTANT_CATEGORY_H
#define ICARUS_COMMON_CONSTANT_CATEGORY_H

#include <cstdint>

namespace ic {

enum class ConstantCategory : uint8_t {
#define IC_XMACRO_TYPE_KIND(kind) kind##Type,
#include "common/language/type_kind.xmacro.h"
  Followup,
  Integer,
  String,
};

}  // namespace ic

#endif  // ICARUS_COMMON_CONSTANT_CATEGORY_H
