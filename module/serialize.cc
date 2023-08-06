#include "module/serialize.h"

#include "absl/cleanup/cleanup.h"
#include "jasmin/serialization.h"
#include "module/type_system.h"
#include "nth/meta/sequence.h"
#include "nth/meta/type.h"
#include "serialization/module.pb.h"
#include "vm/serialization.h"

namespace module {
namespace {

core::Type DeserializeType(serialization::Type const& proto) {
  return core::Type(proto.category(),
                    proto.has_value() ? proto.value() : proto.index());
}

void SerializeType(core::Type t, serialization::Type& proto,
                   bool inline_storage) {
  proto.set_category(t.category());
  if (inline_storage) {
    proto.set_value(t.index());
  } else {
    proto.set_index(t.index());
  }
}

void DeserializeTypeSystem(UniqueId module_id,
                           serialization::TypeSystem const& proto,
                           semantic_analysis::TypeSystem& type_system,
                           semantic_analysis::TypeSystem& current_type_system,
                           serialization::UniqueTypeTable& unique_type_table) {
  for (auto const& parameter_type : proto.parameters()) {
    core::Parameters<core::Type> parameters;
    parameters.reserve(parameter_type.parameters().size());
    for (auto const& p : parameter_type.parameters()) {
      parameters.append(p.name(), core::Type(DeserializeType(p.type())));
    }
    core::ParameterType(type_system, std::move(parameters));
  }

  for (auto const& t : proto.pointers()) {
    core::PointerType(type_system, DeserializeType(t));
  }

  for (auto const& t : proto.buffer_pointers()) {
    semantic_analysis::BufferPointerType(type_system, DeserializeType(t));
  }

  for (auto const& a : proto.arrays()) {
    semantic_analysis::ArrayType(type_system, a.length(),
                                 DeserializeType(a.type()));
  }

  for (auto const& t : proto.slices()) {
    semantic_analysis::SliceType(type_system, DeserializeType(t));
  }

  constexpr size_t ParameterTypeIndex =
      semantic_analysis::TypeSystem::index<core::ParameterType>();

  for (auto const& f : proto.functions()) {
    core::Type parameters(ParameterTypeIndex, f.parameters());
    std::vector<core::Type> returns;
    returns.reserve(f.return_types().size());
    for (auto const& r : f.return_types()) {
      returns.push_back(DeserializeType(r));
    }

    core::FunctionType(type_system,
                       parameters.get<core::ParameterType>(type_system),
                       std::move(returns));
  }

  unique_type_table.insert_enums(module_id, proto.enums());
  size_t i = 0;
  for (auto const& e : proto.enums()) {
    semantic_analysis::EnumType(type_system, module_id, i, &e);
    semantic_analysis::EnumType(current_type_system, module_id, i, &e);
    ++i;
  }
}

}  // namespace

bool SerializeModule(Module const& module, std::ostream& output,
                     serialization::UniqueTypeTable const& unique_type_table,
                     ModuleMap& module_map, GlobalFunctionMap& function_map) {
  serialization::Module proto;

  *proto.mutable_identifier() = module.id().value();
  SerializeTypeSystem(module.type_system(), unique_type_table,
                      *proto.mutable_type_system());

  // TODO: Fix const-correctness in Jasmin.
  vm::SerializationState state(const_cast<Module&>(module).read_only_data(),
                               const_cast<Module&>(module).foreign_symbol_map(),
                               UniqueId::Invalid(), function_map);

  serialization::ForeignSymbolMap::Serialize(module.foreign_symbol_map(),
                                             *proto.mutable_foreign_symbols());

  vm::Serialize(module.initializer(), *proto.mutable_initializer(), state);

  vm::Serialize(module.function_table(), *proto.mutable_function_table(),
                state);

  serialization::ReadOnlyData::Serialize(module.read_only_data(),
                                         *proto.mutable_read_only());
  data_types::Serialize(module.integer_table(), *proto.mutable_integers());

  auto& exported = *proto.mutable_exported();
  for (auto const& [name, symbols] : module.exported_symbols()) {
    auto& exported_symbols = exported[name];
    for (auto const& symbol : symbols) {
      auto& exported_symbol  = *exported_symbols.add_symbols();
      core::Type symbol_type = symbol.type();
      SerializeType(
          symbol_type, *exported_symbol.mutable_symbol_type(),
          module.type_system().has_inline_storage(symbol_type.category()));
      if (symbol_type == semantic_analysis::Type) {
        SerializeType(symbol.as<core::Type>(), *exported_symbol.mutable_type(),
                      module.type_system().has_inline_storage(
                          symbol.as<core::Type>().category()));
      } else if (symbol_type.category() ==
                 module.type_system().index<core::FunctionType>()) {
        serialization::FunctionIndex::Serialize(
            symbol.as<TypedFunction>().function,
            *exported_symbol.mutable_function());
      } else if (symbol_type.category() ==
                     module.type_system()
                         .index<semantic_analysis::PrimitiveType>() or
                 symbol_type.category() ==
                     module.type_system().index<core::SizedIntegerType>()) {
        exported_symbol.set_raw_value(
            symbol.as<TypedValue>().value.raw_value());
      } else {
        NTH_UNIMPLEMENTED();
      }
    }
  }

  return proto.SerializeToOstream(&output);
}

bool DeserializeModuleInto(serialization::Module const& proto,
                           base::PtrSpan<Module const> dependencies,
                           UniqueId module_id, Module& module,
                           semantic_analysis::TypeSystem& current_type_system,
                           serialization::UniqueTypeTable& unique_type_table,
                           ModuleMap& module_map,
                           GlobalFunctionMap& function_map) {
  data_types::Deserialize(proto.integers(), module.integer_table());

  if (not serialization::ReadOnlyData::Deserialize(proto.read_only(),
                                                   module.read_only_data()) or
      not serialization::ForeignSymbolMap::Deserialize(
          proto.foreign_symbols(), module.foreign_symbol_map())) {
    return false;
  }

  DeserializeTypeSystem(module_id, proto.type_system(), module.type_system(),
                        current_type_system, unique_type_table);

  for (auto const& [name, symbols] : proto.exported()) {
    for (auto const& symbol : symbols.symbols()) {
      core::Type symbol_type = DeserializeType(symbol.symbol_type());
      if (symbol_type == semantic_analysis::Type) {
        module.exported_symbols()[name].push_back(
            DeserializeType(symbol.type()));
      } else if (symbol_type.category() ==
                 module.type_system().index<core::FunctionType>()) {
        serialization::FunctionIndex f;
        if (not serialization::FunctionIndex::Deserialize(symbol.function(),
                                                          f)) {
          return false;
        }
        module.exported_symbols()[name].push_back(TypedFunction{
            .type     = symbol_type,
            .function = f,
        });
      } else if (symbol_type.category() ==
                     module.type_system()
                         .index<semantic_analysis::PrimitiveType>() or
                 symbol_type.category() ==
                     module.type_system().index<core::SizedIntegerType>()) {
        jasmin::Value v = jasmin::Value::Uninitialized();
        v.set_raw_value(symbol.raw_value());
        module.exported_symbols()[name].push_back(TypedValue{
            .type  = symbol_type,
            .value = v,
        });
      } else {
        NTH_UNIMPLEMENTED("{}") <<=
            {semantic_analysis::DebugType(symbol_type, module.type_system())};
      }
    }
  }

  vm::SerializationState state(module.read_only_data(),
                               module.foreign_symbol_map(), module_id,
                               function_map);
  if (not vm::Deserialize(proto.function_table(), module.function_table(),
                          module_id, state)) {
    return false;
  }
  vm::Deserialize(proto.initializer(), module.initializer(), state);

  return true;
}

}  // namespace module
