#include "module/resources.h"

namespace module {

Module *Resources::LoadFrom(serialization::Module module) {
  auto m = std::make_unique<Module>();
  serialization::UniqueModuleId id(std::move(*module.mutable_identifier()));
  if (not module::Module::DeserializeInto(std::move(module), *m)) {
    return nullptr;
  }

  module_map_.insert(serialization::ModuleIndex(0),
                     serialization::ModuleIndex(modules_.size()), id);
  auto *ptr = m.get();
  modules_.push_back(std::move(m));
  return ptr;
}

serialization::ModuleIndex Resources::TryLoadModuleByName(
    ModuleName const &name) const {
  return module_map_.get(name_resolver_(name));
}

}  // namespace module
