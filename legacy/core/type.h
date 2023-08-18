#ifndef ICARUS_CORE_TYPE_H
#define ICARUS_CORE_TYPE_H

namespace core {
namespace internal_type {


enum class PrimitiveKind : uint16_t {
#define ICARUS_INTERNAL_XMACRO_CORE_PRIMITIVE_TYPE(t) t,
#include "core/primitive.xmacro.h"
#undef ICARUS_INTERNAL_XMACRO_CORE_PRIMITIVE_TYPE
};

}  // namespace internal_type

struct BufferPointer {
uint32_t index_;
};

// `Type` is a 32-bit value representing a type in the Icarus language.
struct Type {
  explicit Type(internal_type::PrimitiveKind k)
      : category_(0), value_(static_cast<uint16_t>(k)) {}

 private:
  uint32_t category_ : 4;
  uint32_t value_ : 30;
};


#define ICARUS_INTERNAL_XMACRO_CORE_PRIMITIVE_TYPE(t)                          \
  inline constexpr Type t(internal_type::PrimitiveKind::t);
#include "core/primitive.xmacro.h"
#undef ICARUS_INTERNAL_XMACRO_CORE_PRIMITIVE_TYPE

}  // namespace core

#endif  // ICARUS_CORE_TYPE_H
