#include "module/global_module_map.h"

#include "nth/debug/debug.h"

namespace module {

void GlobalModuleMap::insert(serialization::ModuleIndex module_index,
                             serialization::ModuleIndex dep_index,
                             serialization::UniqueModuleId const& value) {
  auto [iter, inserted] = data_.insert(value);
  mapping_.emplace(std::pair(module_index, dep_index),
                   serialization::ModuleIndex(data_.index(iter)));
}

serialization::ModuleIndex GlobalModuleMap::read(
    serialization::ModuleIndex module_index,
    serialization::ModuleIndex dep_index) const {
  if (dep_index == serialization::ModuleIndex::Self()) { return module_index; }
  if (dep_index == serialization::ModuleIndex::Builtin()) { return dep_index; }
  auto iter = mapping_.find(std::pair(module_index, dep_index));
  NTH_ASSERT(iter != mapping_.end());
  return iter->second;
}

serialization::ModuleIndex GlobalModuleMap::index(
    serialization::UniqueModuleId const& id) const {
  auto iter = data_.find(id);
  if (iter == data_.end()) { return serialization::ModuleIndex::Invalid(); }
  return serialization::ModuleIndex(data_.index(iter));
}

void GlobalModuleMap::Serialize(GlobalModuleMap const& from,
                                serialization::proto::ModuleMap& to) {
  for (auto const& id : from.data_) { *to.add_ids() = id.value(); }
}

bool GlobalModuleMap::Deserialize(serialization::ModuleIndex self_index,
                                  serialization::proto::ModuleMap const& from,
                                  GlobalModuleMap& to) {
  size_t i = 0;
  for (auto const & id : from.ids()) {
    to.insert(self_index, serialization::ModuleIndex(i),
              serialization::UniqueModuleId(id));
    ++i;
  }
  return true;
}

}  // namespace module
