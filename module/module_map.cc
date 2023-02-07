#include "module/module_map.h"

#include "base/debug.h"

namespace module {

ModuleMap::IdLookupResult ModuleMap::find(ModuleIndex index) const {
  if (index.value() < ids_.size()) {
    return IdLookupResult(&ids_.from_index(index.value()));
  } else {
    return IdLookupResult(nullptr);
  }
}

Module &ModuleMap::emplace(UniqueModuleId const &id) {
  return ids_.try_emplace(id).first->second;
}

UniqueModuleId const &ModuleMap::operator[](ModuleIndex index) const {
  ASSERT(index.value() < ids_.size());
  return ids_.from_index(index.value()).first;
}

ModuleIndex ModuleMap::TryLoad(ModuleName const &name) const {
  if (auto result = id(name)) {
    return index(result.id());
  } else {
    return ModuleIndex::Invalid();
  }
}

ModuleIndex ModuleMap::index(UniqueModuleId const &id) const {
  return ModuleIndex(ids_.index(id));
}

}  // namespace module
