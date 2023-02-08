#ifndef ICARUS_MODULE_RESOURCES_H
#define ICARUS_MODULE_RESOURCES_H

#include "absl/functional/any_invocable.h"
#include "module/module.h"
#include "module/module_name.h"
#include "serialization/module_map.h"
#include "serialization/read_only_data.h"

namespace module {

struct Resources {
  explicit Resources(
      absl::AnyInvocable<serialization::UniqueModuleId(ModuleName const&) const>
          name_resolver)
      : name_resolver_(std::move(name_resolver)) {
    modules_.push_back(std::make_unique<Module>());
  }

  // Loads the contents of `module` into a new module and returns a pointer to
  // that module if loading was successful. If unsuccessful, a null pointer is
  // returned.
  Module* LoadFrom(serialization::Module module);

  Module& primary_module() { return *modules_[0]; }
  Module& module(serialization::ModuleIndex index) {
    return *modules_[index.value()];
  }

  // Returns the `serialization::ModuleIndex` associated with the given `name`
  // if one exists, and returns `serialization::ModuleIndex::Invalid()`
  // otherwise.
  serialization::ModuleIndex TryLoadModuleByName(ModuleName const& name) const;

 private:
  std::vector<std::unique_ptr<Module>> modules_;
  serialization::ModuleMap module_map_;
  serialization::ReadOnlyData read_only_data_;

  absl::AnyInvocable<serialization::UniqueModuleId(ModuleName const&) const>
      name_resolver_;
};

}  // namespace module

#endif  // ICARUS_MODULE_RESOURCES_H
