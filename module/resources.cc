#include "module/resources.h"

namespace module {

Resources::Resources(
    serialization::UniqueModuleId const &id,
    absl::AnyInvocable<serialization::UniqueModuleId(ModuleName const &) const>
        name_resolver,
    std::unique_ptr<diagnostic::DiagnosticConsumer> diagnostic_consumer)
    : name_resolver_(std::move(name_resolver)),
      diagnostic_consumer_(std::move(diagnostic_consumer)) {
  Populate(id, std::make_unique<Module>(id));
}

Module *Resources::LoadFrom(serialization::Module module) {
  serialization::UniqueModuleId id(std::move(*module.mutable_identifier()));
  auto m = std::make_unique<Module>(id);
  if (not module::Module::DeserializeInto(std::move(module), *m)) {
    return nullptr;
  }

  auto *ptr = m.get();
  Populate(id, std::move(m));
  return ptr;
}

serialization::ModuleIndex Resources::TryLoadModuleByName(
    ModuleName const &name) const {
    return module_map_.get(name_resolver_(name));
}

diagnostic::DiagnosticConsumer &Resources::diagnostic_consumer() {
  return *ASSERT_NOT_NULL(diagnostic_consumer_);
}

std::optional<Resources> Resources::LoadPrimary(serialization::Module module) {
  std::optional<Resources> resources;
  serialization::UniqueModuleId id(std::move(*module.mutable_identifier()));
  auto m = std::make_unique<Module>(id);
  if (not module::Module::DeserializeInto(std::move(module), *m)) {
    return resources;
  }
  resources.emplace(Resources());

  resources->Populate(id, std::move(m));
  return resources;
}

void Resources::Populate(serialization::UniqueModuleId const &id,
                         std::unique_ptr<Module> m) {
  modules_.push_back(std::move(m));
  module_map_.insert(serialization::ModuleIndex(0),
                     serialization::ModuleIndex(modules_.size()), id);
}

}  // namespace module
