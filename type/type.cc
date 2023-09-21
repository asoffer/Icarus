#include "type/type.h"

#include <cstring>

#include "nth/debug/debug.h"

namespace ic::type {

#define IC_XMACRO_TYPE_KIND(k)                                                 \
  static_assert(sizeof(Type) == sizeof(k##Type));                              \
  static_assert(alignof(Type) == alignof(k##Type));                            \
                                                                               \
  k##Type Type::As##k() const {                                                \
    NTH_REQUIRE((v.debug), kind() == Kind::k);                                 \
    k##Type t;                                                                 \
    std::memcpy(&t, this, sizeof(Type));                                       \
    return t;                                                                  \
  }
#include "type/type_kind.xmacro.h"

Type::Kind Type::kind() const { return static_cast<Kind>(data_ >> 56); }

size_t Size(Type t) {
  return 1;
}

}  // namespace ic::type
