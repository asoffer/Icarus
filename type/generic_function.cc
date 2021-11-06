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
    compiler::WorkResources const &wr,
    core::Arguments<Typed<ir::CompleteResultRef>> const &args) const {
  return gen_fn_(wr, args);
}

std::vector<type::Type> GenericFunction::return_types(
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &args) const {
  compiler::WorkResources wr;
  return concrete(wr, args)->return_types(args);
}

}  // namespace type
