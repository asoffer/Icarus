#include "ir/value/generic_fn.h"

#include <vector>

#include "base/global.h"

namespace ir {
namespace {

// Functions held in this global must be invoked while the lock is not being
// held.
base::Global<std::vector<std::unique_ptr<base::any_invocable<Fn(
    compiler::WorkResources const &wr,
    core::Arguments<type::Typed<CompleteResultRef>> const &)>>>>
    gen_fns;

}  // namespace

GenericFn::GenericFn(
    base::any_invocable<
        Fn(compiler::WorkResources const &wr,
                 core::Arguments<type::Typed<CompleteResultRef>> const &)>
        gen) {
  auto handle = gen_fns.lock();
  id_         = handle->size();
  handle->push_back(
      std::make_unique<base::any_invocable<Fn(
          compiler::WorkResources const &wr,
          core::Arguments<type::Typed<CompleteResultRef>> const &)>>(
          std::move(gen)));
}

Fn GenericFn::concrete(
    compiler::WorkResources const &wr,
    core::Arguments<type::Typed<CompleteResultRef>> const &args) const {
  base::any_invocable<Fn(
      compiler::WorkResources const &wr,
      core::Arguments<type::Typed<CompleteResultRef>> const &)> *fn;
  {
    auto handle = gen_fns.lock();
    fn          = (*handle)[id_].get();
  }
  // Call the function without holding the lock.
  return (*fn)(wr, args);
}

}  // namespace ir
