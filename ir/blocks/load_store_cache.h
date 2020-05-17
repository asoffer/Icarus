#ifndef ICARUS_BLOCKS_LOAD_STORE_CACHE_H
#define ICARUS_BLOCKS_LOAD_STORE_CACHE_H

#include "absl/container/flat_hash_map.h"
#include "base/meta.h"
#include "ir/value/addr.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"

namespace ir {

// LoadStoreCache:
//
// While building basic blocks, we cache information that is known about the
// values/registers stored in each allocation on this block. On each load, we
// cache the value loaded so that subsequent loads from the same address can
// reuse this register rather than emitting a redundant load instruction.
//
// Note that this struct is a glorifidy hash-table. It does nothing more than
// track the values cached in it. Users are responsible both for populating and
// clearing caches at the appropriate times.
struct LoadStoreCache {
  // Creates an entry in the cache if one did not already exist, and returns a
  // reference to the value held in the cache whcih can be populated or read
  // from.
  template <typename T>
  Value &slot(RegOr<Addr> r) {
    static_assert(not ir::IsRegOr<T>::value);
    return storage_[base::meta<T>][r];
  }

  // If the template parameter is `void`, clears the entirety of the cache (for
  // all types). This is useful to ensure the cache is invalidated
  // conservatively when we cannot be sure which allocations have escaped. When
  // a non-void type is specified, the cache is cleared only for that type. This
  // is useful primarily within the same block-group.
  //
  // A store to a stack-allocation of type `int64` can only invalidate load
  // caches of the type `int64`. Note that this fact is somewhat subtle. If
  // stack allocations for differing types did not overlap it would be clear: A
  // store should not invalidate stores which we can guarantee it does not
  // overwirte.
  //
  // For variants, the situation is more subtle because two separate types may
  // inhabit the same memory location. Importantly, they cannot do so
  // simultaneously. The problem would occur if there is a cached load of type A
  // followed by a store of type B to the same address (via a variant). A
  // subsequent load of type A would read from the cache. However, this load
  // would be undefined-behavior because an unengaged type would be read. The
  // only way to define the behavior is to ensure that the most recent store to
  // that memory location has the correct type which will invalidate the cache
  // of type A. In other words, this cache takes advantage of this
  // undefined-behavior as an optimization mechanism.
  template <typename T = void>
  void clear() {
    static_assert(not ir::IsRegOr<T>::value);
    if constexpr (base::meta<T> == base::meta<void>) {
      storage_.clear();
    } else {
      storage_.erase(base::meta<T>);
    }
  }

 private:
  absl::flat_hash_map<base::MetaValue, absl::flat_hash_map<RegOr<Addr>, Value>>
      storage_;
};

}  // namespace ir

#endif  // ICARUS_BLOCKS_LOAD_STORE_CACHE_H
