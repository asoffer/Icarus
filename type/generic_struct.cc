#include "type/generic_struct.h"

#include "base/stringify.h"
#include "type/struct.h"

namespace type {

core::Bytes GenericStruct::bytes(core::Arch const &) const {
  return core::Host.pointer().bytes();
}

core::Alignment GenericStruct::alignment(core::Arch const &) const {
  return core::Host.pointer().alignment();
}

}  // namespace type
