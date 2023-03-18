#include "module/resources.h"

namespace module {

Resources::Resources(
    serialization::UniqueModuleId const &id,
    absl::AnyInvocable<serialization::UniqueModuleId(ModuleName const &) const>
        name_resolver,
    std::unique_ptr<diagnostic::DiagnosticConsumer> diagnostic_consumer)
    : primary_module_(id, function_map_),
      name_resolver_(std::move(name_resolver)),
      diagnostic_consumer_(std::move(diagnostic_consumer)) {}

Module &Resources::AllocateModule(serialization::UniqueModuleId const &id) {
  return *modules_.emplace_back(std::make_unique<Module>(id, function_map_));
}

serialization::ModuleIndex Resources::TryLoadModuleByName(
    ModuleName const &name) const {
  return module_map_.index(name_resolver_(name));
}

diagnostic::DiagnosticConsumer &Resources::diagnostic_consumer() {
  return *ASSERT_NOT_NULL(diagnostic_consumer_);
}

core::Type Resources::Translate(core::Type type,
                                semantic_analysis::TypeSystem &from,
                                semantic_analysis::TypeSystem &to) const {
  namespace sa = semantic_analysis;
  switch (type.category()) {
    case sa::TypeSystem::index<sa::OpaqueType>(): // TODO this is in general wrong.
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
  UNREACHABLE(DebugType(type, from));
}

Symbol Resources::TranslateToPrimary(serialization::ModuleIndex from,
                                     Symbol const &symbol) {
  auto &m = module(from);
  core::Type type =
      Translate(symbol.type(), m.type_system(), primary_module().type_system());
  if (type == semantic_analysis::Type) {
    return Symbol(Translate(symbol.as<core::Type>(), module(from).type_system(),
                            primary_module().type_system()));
  } else if (type.is<core::FunctionType>(primary_module().type_system())) {
    auto fn_index = primary_module()
                        .Wrap(&m.function_table().function(
                            symbol.as<TypedFunction>().function))
                        .first;
    return Symbol(TypedFunction{
        .type     = type,
        .function = fn_index,
    });
  } else if (type.category() ==
             m.type_system().index<semantic_analysis::PrimitiveType>()) {
    return symbol;
  } else {
    NOT_YET(semantic_analysis::DebugType(type, primary_module().type_system()));
  }
}

}  // namespace module
