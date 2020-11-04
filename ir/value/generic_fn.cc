#include "ir/value/generic_fn.h"

#include <vector>

#include "base/global.h"
#include "ir/value/native_fn.h"
#include "ir/value/value.h"

namespace ir {
namespace {

base::Global<std::vector<
    base::any_invocable<NativeFn(core::Arguments<type::Typed<Value>> const &)>>>
    gen_fns;

}  // namespace

GenericFn::GenericFn(
    base::any_invocable<NativeFn(core::Arguments<type::Typed<Value>> const &)>
        gen) {
  auto handle = gen_fns.lock();
  id_         = handle->size();
  handle->push_back(std::move(gen));
}

NativeFn GenericFn::concrete(
    core::Arguments<type::Typed<Value>> const &args) const {
  auto handle = gen_fns.lock();
  return (*handle)[id_](args);
}

}  // namespace ir
