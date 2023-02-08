#ifndef ICARUS_SERIALIZATION_READ_ONLY_DATA_H
#define ICARUS_SERIALIZATION_READ_ONLY_DATA_H

#include <string>
#include <string_view>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "nth/container/flyweight_set.h"
#include "serialization/module_index.h"
#include "serialization/proto/read_only_data.pb.h"

namespace serialization {

// Represents a collection of read-only data merged from all `ReadOnlyData`
// protos passed to `merge`.
struct ReadOnlyDataAggregator {
  // Merges data from `data` into `*this`, ensuring that values already present
  // are deduplicated.
  void merge(ModuleIndex module_index, ReadOnlyData const& data);

  // Inserts a `value` associated with `module_index` in position `index` into
  // the aggregator, so that it can later be looked up via these indices.
  void insert(ModuleIndex module_index, size_t index, std::string const& value);

  // Given the module-specific index represented by `module_index` and `index`,
  // returns a pair consisting of the globally unique index and a view of the
  // associated value.
  std::pair<size_t, std::string_view> read(ModuleIndex module_index,
                                           size_t index) const;

  // Writes the contents of `*this`
  void write_to(ReadOnlyData& output);

 private:
  nth::flyweight_set<std::string> data_;
  absl::flat_hash_map<std::pair<ModuleIndex, size_t>, size_t> mapping_;
};

}  // namespace serialization

#endif  // ICARUS_SERIALIZATION_READ_ONLY_DATA_H
