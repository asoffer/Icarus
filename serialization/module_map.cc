#include "serialization/module_map.h"

#include "base/debug.h"

namespace serialization {

void ModuleMap::insert(ModuleIndex module_index, ModuleIndex dep_index,
                       UniqueModuleId const& value) {
  auto [iter, inserted] = data_.insert(value);
  mapping_.emplace(std::pair(module_index, dep_index),
                   ModuleIndex(data_.index(iter)));
}

std::pair<ModuleIndex, UniqueModuleId const&> ModuleMap::read(
    ModuleIndex module_index, ModuleIndex dep_index) const {
  auto iter = mapping_.find(std::pair(module_index, dep_index));
  ASSERT(iter != mapping_.end());
  return std::pair<ModuleIndex, UniqueModuleId const&>(
      iter->second, data_.from_index(iter->second.value()));
}

ModuleIndex ModuleMap::get(UniqueModuleId const& id) const {
  auto iter = data_.find(id);
  if (iter == data_.end()) { return ModuleIndex::Invalid(); }
  return ModuleIndex(data_.index(iter));
}

}  // namespace serialization
