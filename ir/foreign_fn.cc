#include "ir/foreign_fn.h"

#include "base/debug.h"
#include "absl/container/flat_hash_map.h"
#include "base/guarded.h"

namespace ir {

static base::guarded<absl::flat_hash_map<void (*)(), type::Function const *>>
    foreign_fns;

ForeignFn::ForeignFn(void (*fn)(), type::Function const *t) : fn_(fn) {
  // TODO what if two calls to foreign claim it's a different type? Should this
  // be allowed? Is it already checked?
  //
  // More thoughts: yes it's allowed...
  //    allocate ::= (T :: type, num: int32) -> [*]T {
  //      malloc ::= foreign("malloc", nat64 -> [*]T)
  //      return malloc(T'bytes * (num as nat64))
  //    }
  foreign_fns.lock()->emplace(fn, t);
}

type::Function const *ForeignFn::type() const {
  auto handle = foreign_fns.lock();
  auto iter   = handle->find(fn_);
  ASSERT(iter != handle->end());
  return iter->second;
}

}  // namespace ir
