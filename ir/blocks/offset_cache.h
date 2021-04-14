#ifndef ICARUS_BLOCKS_OFFSET_CACHE_H
#define ICARUS_BLOCKS_OFFSET_CACHE_H

#include "absl/container/flat_hash_map.h"
#include "ir/value/addr.h"
#include "ir/value/reg_or.h"

namespace ir {

// OffsetCache:
//
// While building basic blocks, we cache information that is known about the
// registers offset relative to one another. For each instruction that assigns a
// register to be an offset relative to another, we cache the result so that
// subsequent computations can use the same register.
//
// Note that this struct is a glorifidy hash-table. It does nothing more than
// track the values cached in it. Users are responsible both for populating and
// clearing caches at the appropriate times.
struct OffsetCache {
  enum Kind {
    // Assumes we are pointing into a buffer and will cache index results passed
    // this element in the buffer.
    Passed,
    // Assumes we are pointing at a struct and will cache index results into
    // that struct.
    Into,
  };
  // Creates an entry in the cache if one did not already exist, and returns a
  // reference to the value held in the cache whcih can be populated or read
  // from.
  std::optional<Reg> get(RegOr<Addr> original, RegOr<int64_t> offset,
                         Kind kind) const {
    auto iter = storage_.find(std::make_tuple(original, offset, kind));
    if (iter == storage_.end()) { return std::nullopt; }
    return iter->second;
  }

  void set(RegOr<Addr> original, RegOr<int64_t> offset, Kind kind, Reg result) {
    storage_.emplace(std::make_tuple(original, offset, kind), result);
  }

 private:
  absl::flat_hash_map<std::tuple<RegOr<Addr>, RegOr<int64_t>, Kind>, Reg>
      storage_;
};

}  // namespace ir

#endif  // ICARUS_BLOCKS_LOAD_STORE_CACHE_H
