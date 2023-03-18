#include "module/global_index_map.h"

namespace module {

void GlobalIndexMap::insert(size_t from_index,
                            serialization::ModuleIndex module_index,
                            size_t index) {
  indices_.try_emplace(from_index, module_index, index);
  reverse_indices_.try_emplace(std::pair(module_index, index), from_index);
}

std::pair<serialization::ModuleIndex, size_t> GlobalIndexMap::find(
    size_t from_index) const {
  if (auto iter = indices_.find(from_index); iter == indices_.end()) {
    return std::pair(serialization::ModuleIndex::Invalid(),
                     std::numeric_limits<size_t>::max());
  } else {
    return iter->second;
  }
}

size_t GlobalIndexMap::find(serialization::ModuleIndex m, size_t index) const {
  auto iter = reverse_indices_.find(std::pair(m, index));
  if (iter == reverse_indices_.end()) {
    return std::numeric_limits<size_t>::max();
  }
  return iter->second;
}

}  // namespace module
