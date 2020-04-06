#include "ir/value/generic_fn.h"

#include <vector>

#include "base/guarded.h"
#include "ir/value/value.h"

namespace ir {
namespace {

base::guarded<std::vector<
    std::function<NativeFn(core::FnArgs<type::Typed<Value>> const&)>>>
    gen_fns;

}  // namespace

GenericFn::GenericFn(
    std::function<NativeFn(core::FnArgs<type::Typed<Value>> const&)> gen) {
  auto handle = gen_fns.lock();
  id_         = handle->size();
  handle->push_back(std::move(gen));
}

}  // namespace ir
