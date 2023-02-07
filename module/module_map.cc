#include "module/module_map.h"

namespace module {

ModuleMap::IdLookupResult ModuleMap::find(ModuleIndex index) const {
  if (index.value() < ids_.size()) {
    return IdLookupResult(&ids_.from_index(index.value()));
  } else {
    return IdLookupResult(nullptr);
  }
}

std::span<FilePath const> ModuleMap::paths(UniqueModuleId const &id) {
  return paths(index(id));
}

std::span<FilePath const> ModuleMap::paths(ModuleIndex index) {
  return ids_.from_index(index.value()).second;
}

}  // namespace module
