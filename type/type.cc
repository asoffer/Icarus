#include "type/type.h"

namespace type {

Type const *Prim(BasicType b) {
  switch (b) {
#define PRIMITIVE_MACRO(EnumName, name)                                        \
  case BasicType::EnumName:                                                    \
    return EnumName;
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
  }
  UNREACHABLE();
}

}  // namespace type
