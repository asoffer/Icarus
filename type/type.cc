#include "type/type.h"

#include "type/array.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/variant.h"

namespace type {

bool Type::is_big() const {
  return is<Array>() or is<Struct>() or is<Variant>() or is<Tuple>();
}

}  // namespace type
