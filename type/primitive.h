#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

#include "type/basic.h"

namespace ic::type {

// Represents a primitive type built-in to the language.
struct PrimitiveType : internal_type::BasicType {
  enum class Kind : uint8_t {
#define IC_XMACRO_PRIMITIVE_TYPE_BEGIN_CATEGORY(name)                          \
  InternalBeginCategory_##name,
#define IC_XMACRO_PRIMITIVE_TYPE_END_CATEGORY(name) InternalEndCategory_##name,
#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling) kind,
#include "common/language/primitive_types.xmacro.h"
  };

  explicit constexpr PrimitiveType(Kind k)
      : internal_type::BasicType(Type::Kind::Primitive,
                                 static_cast<uint64_t>(k)) {}

  Kind kind() const { return static_cast<Kind>(data() & 0xff); }

  friend void NthPrint(auto& p, auto& f, PrimitiveType t) {
    switch (t.kind()) {
#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling)                       \
  case Kind::kind:                                                             \
    p.write(spelling);                                                         \
    return;
#include "common/language/primitive_types.xmacro.h"
      default: NTH_UNREACHABLE();
    }
  }

 private:
  friend Type;
  PrimitiveType() = default;
};

#define IC_XMACRO_PRIMITIVE_TYPE_BEGIN_CATEGORY(category_name)                 \
  bool category_name(PrimitiveType);
#include "common/language/primitive_types.xmacro.h"

#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling)                       \
  inline Type const symbol = PrimitiveType(PrimitiveType::Kind::kind);
#include "common/language/primitive_types.xmacro.h"

}  // namespace ic::type

#endif  // ICARUS_TYPE_PRIMITIVE_H
