#ifndef ICARUS_BLOCKS_LOAD_STORE_CACHE_H
#define ICARUS_BLOCKS_LOAD_STORE_CACHE_H

#include "absl/container/flat_hash_map.h"
#include "base/meta.h"
#include "base/unaligned_ref.h"
#include "base/untyped_buffer.h"
#include "ir/value/addr.h"
#include "ir/value/reg_or.h"

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
  // pair consisting of a reference to the value held in the cache which can be
  // populated or read from, along with a bool indicating whether or not the
  // slot was newly inserted.
  template <typename T>
  std::pair<base::unaligned_ref<RegOr<T>>, bool> slot(RegOr<addr_t> r) {
    constexpr auto type = base::meta<T>;
    static_assert(not type.template is_a<ir::RegOr>());
    auto& map_for_type    = storage_[type];
    auto [iter, inserted] = map_for_type.try_emplace(r);
    if (inserted) { iter->second.append(RegOr<T>()); }
    return std::pair(iter->second.begin().template read<RegOr<T>>(), inserted);
  }

  // If the template parameter is `void`, clears the entirety of the cache (for
  // all types). This is useful to ensure the cache is invalidated
  // conservatively when we cannot be sure which allocations have escaped. When
  // a non-void type is specified, the cache is cleared only for that type. This
  // is useful primarily within the same subroutine.
  //
  // A store to a stack-allocation of type `i64` can only invalidate load
  // caches of the type `i64`. Note that this fact is somewhat subtle. If
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
    constexpr auto type = base::meta<T>;
    static_assert(not type.template is_a<ir::RegOr>());
    if constexpr (type == base::meta<void>) {
      storage_.clear();
    } else {
      storage_.erase(base::meta<T>);
    }
  }

  void clear(base::MetaValue m) { storage_.erase(m); }

 private:
  absl::flat_hash_map<base::MetaValue,
                      absl::flat_hash_map<RegOr<addr_t>, base::untyped_buffer>>
      storage_;
};

}  // namespace ir

#endif  // ICARUS_BLOCKS_LOAD_STORE_CACHE_H
