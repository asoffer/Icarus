#ifndef ICARUS_MODULE_SPECIFIED_MODULE_MAP_H
#define ICARUS_MODULE_SPECIFIED_MODULE_MAP_H

#include "absl/container/flat_hash_map.h"
#include "module/module_map.h"

namespace module {
// Represents a `ModuleMap` in which there is a unique name used for each module
// and that name is specified directly on this map.
struct SpecifiedModuleMap : ModuleMap {
  IdLookupResult id(ModuleName const& name) const override {
    auto iter = specification_.find(name);
    if (iter == specification_.end()) {
      return IdLookupResult();
    } else {
      return find(index(iter->second));
    }
  }

  void identify(ModuleName const& name, UniqueModuleId const& id) {
    specification_.emplace(name, id);
    emplace(id);
  }

 private:
  absl::flat_hash_map<ModuleName, UniqueModuleId> specification_;
};

}  // namespace module

#endif  // ICARUS_MODULE_SPECIFIED_MODULE_MAP_H
