#include "type/generic_function.h"

namespace type {
Type const *Generic = new GenericFunction;

core::Bytes GenericFunction::bytes(core::Arch const &) const {
  return core::Host.pointer().bytes();
}

core::Alignment GenericFunction::alignment(core::Arch const &) const {
  return core::Host.pointer().alignment();
}
}  // namespace type
