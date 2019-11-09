#include "type/type.h"

namespace type {

bool Type::is_big() const {
  // TODO implement with virtual functions
  return false;
  // return is<Array>() or is<Struct>() or is<Variant>() or is<Tuple>();
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
