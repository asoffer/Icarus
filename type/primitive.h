#ifndef ICARUS_TYPE_PRIMITIVE_H
#define ICARUS_TYPE_PRIMITIVE_H

namespace ic::type {
// Represents a primitive type built-in to the language.
struct PrimitiveType : Type {
  enum class Kind : uint8_t {
#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling) kind,
#define IC_XMACRO_PRIMITIVE_TYPE_CATEGORY_RANGE(name, start, end)              \
  InternalBeginCategory_##name = static_cast<uint8_t>(Kind::start),            \
  InternalEndCategory_##name   = static_cast<uint8_t>(Kind::end) + 1,
#include "common/language/primitive_types.xmacro.h"
  };

  explicit constexpr PrimitiveType(Kind k)
      : Type(Type::Kind::Primitive, static_cast<uint64_t>(k)) {}

  PrimitiveType::Kind primitive_kind() const {
    return static_cast<PrimitiveType::Kind>(index() & 0xff);
  }

  friend void NthPrint(auto& p, auto& f, PrimitiveType t) {
    switch (t.primitive_kind()) {
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
  explicit PrimitiveType() = default;
  explicit constexpr PrimitiveType(uint32_t n)
      : Type(Type::Kind::Primitive, n) {}
};

#define IC_XMACRO_PRIMITIVE_TYPE_CATEGORY_RANGE(category_name, start, end)     \
  inline bool category_name(PrimitiveType p) {                                 \
    return static_cast<uint8_t>(PrimitiveType::Kind::start) <=                 \
               static_cast<uint8_t>(p.primitive_kind()) and                    \
           static_cast<uint8_t>(p.primitive_kind()) <=                         \
               static_cast<uint8_t>(PrimitiveType::Kind::end);                 \
  }
#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling)                       \
  inline PrimitiveType const symbol = PrimitiveType(PrimitiveType::Kind::kind);
#include "common/language/primitive_types.xmacro.h"

}  // namespace ic::type

#endif  // ICARUS_TYPE_PRIMITIVE_H
