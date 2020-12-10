#include "ir/value/foreign_fn.h"

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "base/flyweight_map.h"
#include "base/guarded.h"

namespace ir {

namespace {

// Note: We store both the foreign function pointer and it's type. This means
// that we could have the same foreign function multiple times with different
// types. This is intentional and can occur in two contexts. First, because
// named parameters are part of the type, we could easily have all of the
// following:
//
// ```
// foreign("malloc", (num_bytes: u64) -> [*]i64)
// foreign("malloc", (bytes_to_alloc: u64) -> [*]i64)
// foreign("malloc", u64 -> [*]i64)
// ```
//
// Second, in generic contexts, the return type may be different:
//
// ```
// allocate ::= (T :: type, num: i32) -> [*]T {
//   malloc ::= foreign("malloc", u64 -> [*]T)
//   return malloc(T'bytes * (num as u64))
// }
// ```
//
struct ForeignFnData {
  void (*fn)();
  type::Function const *type;

  template <typename H>
  friend H AbslHashValue(H h, ForeignFnData data) {
    return H::combine(std::move(h), data.fn, data.type);
  }

  friend constexpr bool operator==(ForeignFnData lhs, ForeignFnData rhs) {
    return lhs.fn == rhs.fn and lhs.type == rhs.type;
  }
};

base::guarded<base::flyweight_map<ForeignFnData>> foreign_fns;

}  // namespace

ForeignFn::ForeignFn(void (*fn)(), type::Function const *t)
    : id_(foreign_fns.lock()->get(ForeignFnData{fn, t})) {}

type::Function const *ForeignFn::type() const {
  return foreign_fns.lock()->get(id_).type;
}

ForeignFn::void_fn_ptr ForeignFn::get() const {
  return foreign_fns.lock()->get(id_).fn;
}

}  // namespace ir
