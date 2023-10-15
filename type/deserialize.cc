#include "type/deserialize.h"

#include "nth/debug/debug.h"
#include "type/type.h"

namespace ic::type {

Type Deserialize(TypeProto const& proto) {
  switch (proto.kind()) {
    case TypeProto::PRIMITIVE:
      return type::PrimitiveType(
          static_cast<type::PrimitiveType::Kind>(proto.index()));
    default:
      NTH_UNREACHABLE();
  }
}

}  // namespace ic::type
