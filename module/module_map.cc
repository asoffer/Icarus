#include "module/module_map.h"

#include "base/debug.h"

namespace module {

ModuleMap::IdLookupResult ModuleMap::find(
    serialization::ModuleIndex index) const {
  if (index.value() < ids_.size()) {
    return IdLookupResult(&ids_.from_index(index.value()));
  } else {
    return IdLookupResult(nullptr);
  }
}

Module &ModuleMap::emplace(UniqueModuleId const &id) {
  return ids_.try_emplace(id).first->second;
}

UniqueModuleId const &ModuleMap::operator[](
    serialization::ModuleIndex index) const {
  ASSERT(index.value() < ids_.size());
  return ids_.from_index(index.value()).first;
}

serialization::ModuleIndex ModuleMap::TryLoad(ModuleName const &name) const {
  if (auto result = id(name)) {
    return index(result.id());
  } else {
    return serialization::ModuleIndex::Invalid();
  }
}

serialization::ModuleIndex ModuleMap::index(UniqueModuleId const &id) const {
  return serialization::ModuleIndex(ids_.index(id));
}

}  // namespace module
