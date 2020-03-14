#include "type/generic_function.h"

#include "type/function.h"

namespace type {

core::Bytes GenericFunction::bytes(core::Arch const &) const {
  return core::Host.pointer().bytes();
}

core::Alignment GenericFunction::alignment(core::Arch const &) const {
  return core::Host.pointer().alignment();
}

Function const *GenericFunction::concrete(
    core::FnArgs<Typed<ir::Results>> const &args) const {
  return gen_fn_(args);
}

}  // namespace type
