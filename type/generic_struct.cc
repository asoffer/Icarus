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

Struct const *GenericStruct::concrete(
    core::FnArgs<Typed<ir::Value>> const &args) const {
  return gen_(args);
}

}  // namespace type
