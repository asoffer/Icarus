#include "type/type.h"

#include "type/array.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace type {

bool Type::is_big() const {
  return is<Array>() or is<Struct>() or is<Variant>() or is<Tuple>();
}

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
