#include "module/global_index_map.h"

#include "base/debug.h"

namespace module {

void GlobalIndexMap::insert(size_t from_index,
                            serialization::ModuleIndex module_index,
                            size_t index) {
  indices_.try_emplace(from_index, module_index, index);
  reverse_indices_.try_emplace(std::pair(module_index, index), from_index);
}

std::pair<serialization::ModuleIndex, size_t> GlobalIndexMap::find(
    size_t from_index) const {
  auto iter = indices_.find(from_index);
  ASSERT(iter != indices_.end());
  return iter->second;
}

size_t GlobalIndexMap::find(serialization::ModuleIndex m, size_t index) const {
  auto iter = reverse_indices_.find(std::pair(m, index));
  ASSERT(iter != reverse_indices_.end());
  return iter->second;
}

}  // namespace module
