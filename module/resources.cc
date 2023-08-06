#include "module/resources.h"

#include "nth/debug/debug.h"

namespace module {

Resources::Resources(UniqueId id, ModuleMap &module_map,
                     diagnostic::DiagnosticConsumer &diagnostic_consumer)
    : primary_module_(id, function_map_),
      module_map_(module_map),
      diagnostic_consumer_(diagnostic_consumer) {}

Module &Resources::module(UniqueId module_id) {
  for (auto *m : module_map().imported_modules()) {
    if (m->id() == module_id) { return *m; }
  }
  NTH_UNREACHABLE("Unable to find module {}") <<= {module_id};
}

UniqueId Resources::TryLoadModuleByName(ModuleName const &name) const {
  return module_map_.name_resolver()(name);
}

diagnostic::DiagnosticConsumer &Resources::diagnostic_consumer() {
  return diagnostic_consumer_;
}

core::Type Resources::Translate(core::Type type, UniqueId module_id,
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
        param.value = Translate(param.value, module_id, from, to);
      }
      return core::ParameterType(to, std::move(parameters));
    }
    case sa::TypeSystem::index<core::PointerType>():
      return core::PointerType(
          to, Translate(type.get<core::PointerType>(from).pointee(),
                        module_id, from, to));
    case sa::TypeSystem::index<sa::BufferPointerType>():
      return sa::BufferPointerType(
          to, Translate(type.get<sa::BufferPointerType>(from).pointee(),
                        module_id, from, to));
    case sa::TypeSystem::index<sa::ArrayType>(): NTH_UNIMPLEMENTED(); break;
    case sa::TypeSystem::index<sa::SliceType>():
      return sa::SliceType(
          to, Translate(type.get<sa::SliceType>(from).pointee(), module_id,
                        from, to));
    case sa::TypeSystem::index<core::FunctionType>(): {
      auto f = type.get<core::FunctionType>(from);
      std::vector<core::Type> return_types;
      return_types.reserve(f.returns().size());
      for (core::Type t : f.returns()) {
        return_types.push_back(Translate(t, module_id, from, to));
      }
      return core::FunctionType(
          to,
          Translate(f.parameter_type(), module_id, from, to)
              .get<core::ParameterType>(to),
          std::move(return_types));
    }
    case sa::TypeSystem::index<sa::EnumType>(): {
      auto [from_index, enum_index, ptr] =
          type.get<sa::EnumType>(from).decompose();
      // TODO: I think this module index is wrong.
      return sa::EnumType(to, module_id, enum_index, ptr);
    }
    case sa::TypeSystem::index<sa::OpaqueType>(): {
      NTH_UNIMPLEMENTED();
      // return core::Type(sa::TypeSystem::index<sa::OpaqueType>(),
      //                   opaque_map_.find(module_id, type.index()));
    }
  }
  NTH_UNREACHABLE("{}") <<= {DebugType(type, from)};
}

Symbol Resources::TranslateToPrimary(UniqueId from, Symbol const &symbol) {
  auto &m         = module(from);
  core::Type type = Translate(symbol.type(), from, m.type_system(),
                              primary_module().type_system());
  if (type == semantic_analysis::Type) {
    return Symbol(Translate(symbol.as<core::Type>(), from,
                            module(from).type_system(),
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
    NTH_UNIMPLEMENTED("{}") <<=
        {semantic_analysis::DebugType(type, primary_module().type_system())};
  }
}

}  // namespace module
