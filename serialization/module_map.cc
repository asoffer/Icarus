#include "serialization/module_map.h"

#include "base/debug.h"

namespace serialization {

ModuleIndex ModuleMap::index(UniqueModuleId const& id) const {
  auto iter = data_.find(id);
  if (iter == data_.end()) { return ModuleIndex::Invalid(); }
  return ModuleIndex(data_.index(iter));
}

UniqueModuleId const& ModuleMap::id(ModuleIndex index) const {
  return data_.from_index(index.value());
}

void ModuleMap::Serialize(ModuleMap const & from, proto::ModuleMap& to) {
  for (auto const& id : from.data_) { *to.add_ids() = id.value(); }
}

bool ModuleMap::Deserialize(proto::ModuleMap const& from, ModuleMap& to) {
  for (auto const& id : from.ids()) {
    auto [iter, inserted] = to.data_.insert(UniqueModuleId(id));
    if (not inserted) { return false; }
  }
  return true;
}

std::pair<ModuleIndex, bool> ModuleMap::insert(UniqueModuleId const& id) {
  auto [iter, inserted] = data_.insert(id);
  return std::pair(ModuleIndex(data_.index(iter)), inserted);
}

}  // namespace serialization
