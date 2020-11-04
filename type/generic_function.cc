#include "type/generic_function.h"

#include "base/stringify.h"
#include "type/function.h"

namespace type {

core::Bytes GenericFunction::bytes(core::Arch const &) const {
  return core::Host.pointer().bytes();
}

core::Alignment GenericFunction::alignment(core::Arch const &) const {
  return core::Host.pointer().alignment();
}

Function const *GenericFunction::concrete(
    core::Arguments<Typed<ir::Value>> const &args) const {
  return gen_fn_(args);
}

std::vector<type::Type> GenericFunction::return_types(
    core::Arguments<type::Typed<ir::Value>> const &args) const {
  return concrete(args)->return_types(args);
}

}  // namespace type
