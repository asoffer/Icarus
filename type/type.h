#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <cstdint>
#include <cstring>

namespace ic::type {

#define IC_XMACRO_TYPE_KIND(kind) struct kind##Type;
#include "type/type_kind.xmacro.h"

struct Type {
  enum class Kind : uint8_t {
#define IC_XMACRO_TYPE_KIND(kind) kind,
#include "type/type_kind.xmacro.h"
  };

  friend void NthPrint(auto& p, Kind k) {
    switch (k) {
#define IC_XMACRO_TYPE_KIND(kind)                                              \
  case Kind::kind:                                                             \
    p.write("Type::Kind::");                                                   \
    p.write(#kind);                                                            \
    break;
#include "type/type_kind.xmacro.h"
    }
  }

  Kind kind() const;

  friend bool operator==(Type, Type) = default;
  friend bool operator!=(Type, Type) = default;

#define IC_XMACRO_TYPE_KIND(kind)                                              \
  constexpr Type(kind##Type t);                                                \
  kind##Type As##kind() const;
#include "type/type_kind.xmacro.h"

 private:
  Type() = default;
  uint64_t data_;
};

struct PrimitiveType {
  enum class Kind : uint8_t { Bool, Integer, Type };

  explicit constexpr PrimitiveType(Kind k)
      : data_((static_cast<uint64_t>(Type::Kind::Primitive) << 56) |
               static_cast<uint64_t>(k)) {}

  friend bool operator==(PrimitiveType, PrimitiveType) = default;
  friend bool operator!=(PrimitiveType, PrimitiveType) = default;

  Kind kind() const { return static_cast<Kind>(data_ & 0xff); }

 private:
  friend Type;
  PrimitiveType() = default;

  uint64_t data_;
};

#define IC_XMACRO_TYPE_KIND(kind)                                              \
  inline constexpr Type::Type(kind##Type t) : data_(t.data_) {}
#include "type/type_kind.xmacro.h"

inline constexpr Type Bool    = PrimitiveType(PrimitiveType::Kind::Bool);
inline constexpr Type Integer = PrimitiveType(PrimitiveType::Kind::Integer);
inline constexpr Type Type_   = PrimitiveType(PrimitiveType::Kind::Type);

}  // namespace ic::type

#endif  // ICARUS_TYPE_TYPE_H
