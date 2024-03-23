#ifndef ICARUS_TYPE_OPAQUE_H
#define ICARUS_TYPE_OPAQUE_H

#include "type/type.h"

namespace ic::type {

struct OpaqueType : Type {
  friend void NthPrint(auto&, auto&, OpaqueType) { NTH_UNREACHABLE(); }
};


OpaqueType Opaque();

}  // namespace ic::type

#endif  // ICARUS_TYPE_OPAQUE_H
