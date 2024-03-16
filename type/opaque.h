#ifndef ICARUS_TYPE_OPAQUE_H
#define ICARUS_TYPE_OPAQUE_H

#include "type/basic.h"

namespace ic::type {

#if 0
struct OpaqueType : internal_type::BasicType {
  friend void NthPrint(auto& p, auto& fmt, OpaqueType o) {
    p.write("opaque.");
    fmt(p, type::Type(o).index());
  }

 private:
  friend Type;
  friend OpaqueType Opaque();

  explicit OpaqueType() = default;
  explicit constexpr OpaqueType(uint64_t n)
      : BasicType(Type::Kind::Opaque, n) {}
};

#endif
OpaqueType Opaque();

}  // namespace ic::type

#endif  // ICARUS_TYPE_OPAQUE_H
