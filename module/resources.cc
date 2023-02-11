#include "module/resources.h"

namespace module {

Resources::Resources(
    serialization::UniqueModuleId const &id,
    absl::AnyInvocable<serialization::UniqueModuleId(ModuleName const &) const>
        name_resolver,
    std::unique_ptr<diagnostic::DiagnosticConsumer> diagnostic_consumer)
    : primary_module_(id),
      name_resolver_(std::move(name_resolver)),
      diagnostic_consumer_(std::move(diagnostic_consumer)) {}

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
  return primary_module().map().index(name_resolver_(name));
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
  Module &module                = primary_module();
  serialization::ModuleMap &map = module.map();
  auto [index, inserted]        = map.insert(id);
  ASSERT(inserted);
  ASSERT(index.value() == modules_.size());
  modules_.push_back(std::move(m));
}

core::Type Resources::Translate(core::Type type,
                                semantic_analysis::TypeSystem &from,
                                semantic_analysis::TypeSystem &to) const {
  namespace sa = semantic_analysis;
  switch (type.category()) {
    case sa::TypeSystem::index<sa::PrimitiveType>():
    case sa::TypeSystem::index<core::SizedIntegerType>(): return type;
    case sa::TypeSystem::index<core::ParameterType>(): {
      core::Parameters<core::Type> parameters =
          type.get<core::ParameterType>(from).value();
      for (auto &param : parameters) {
        param.value = Translate(param.value, from, to);
      }
      return core::ParameterType(to, std::move(parameters));
    }
    case sa::TypeSystem::index<core::PointerType>():
      return core::PointerType(
          to, Translate(type.get<core::PointerType>(from).pointee(), from, to));
    case sa::TypeSystem::index<sa::BufferPointerType>():
      return sa::BufferPointerType(
          to,
          Translate(type.get<sa::BufferPointerType>(from).pointee(), from, to));
    case sa::TypeSystem::index<sa::ArrayType>(): NOT_YET(); break;
    case sa::TypeSystem::index<sa::SliceType>():
      return sa::SliceType(
          to, Translate(type.get<sa::SliceType>(from).pointee(), from, to));
    case sa::TypeSystem::index<core::FunctionType>(): {
      auto f = type.get<core::FunctionType>(from);
      std::vector<core::Type> return_types;
      return_types.reserve(f.returns().size());
      for (core::Type t : f.returns()) {
        return_types.push_back(Translate(t, from, to));
      }
      return core::FunctionType(
          to,
          Translate(f.parameter_type(), from, to).get<core::ParameterType>(to),
          std::move(return_types));
    }
  }
  UNREACHABLE();
}

data_types::Fn Resources::TranslateToPrimary(data_types::Fn f) {
  auto& m = module(f.module());
  auto const *fn =  m.function(f.local());
  NOT_YET(); // Expose wrapped function.
}

}  // namespace module
