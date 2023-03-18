#ifndef ICARUS_MODULE_GLOBAL_INDEX_MAP_H
#define ICARUS_MODULE_GLOBAL_INDEX_MAP_H

#include <utility>

#include "absl/container/flat_hash_map.h"
#include "serialization/module_index.h"

namespace module {

struct GlobalIndexMap {
  void insert(size_t from_index, serialization::ModuleIndex module_index,
              size_t index);

  std::pair<serialization::ModuleIndex, size_t> find(size_t from_index) const;

  size_t find(serialization::ModuleIndex m, size_t index) const;

  auto const& reverse_entries() { return reverse_indices_; }

 private:
  absl::flat_hash_map<size_t, std::pair<serialization::ModuleIndex, size_t>>
      indices_;
  absl::flat_hash_map<std::pair<serialization::ModuleIndex, size_t>, size_t>
      reverse_indices_;
};

}  // namespace module

#endif  // ICARUS_MODULE_GLOBAL_INDEX_MAP_H
