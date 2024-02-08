#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <cstdint>
#include <tuple>
#include <utility>
#include <vector>

#include "type/type_system.h"

namespace ic::type {

void NthPrint(auto& p, auto& f, Type t) {
  switch (t.kind()) {
#define IC_XMACRO_TYPE_KIND(kind)                                              \
  case Type::Kind::kind:                                                       \
    f(p, t.As##kind());                                                        \
    return;
#include "common/language/type_kind.xmacro.h"
  }
}

TypeSystem& GlobalTypeSystem();

}  // namespace ic::type

#endif  // ICARUS_TYPE_TYPE_H
