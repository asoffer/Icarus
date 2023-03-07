#include "module/module.h"

#include "jasmin/serialization.h"
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

void SerializeTypeSystem(semantic_analysis::TypeSystem& type_system,
                         serialization::TypeSystem& proto) {
  type_system.visit_all_stored([&](auto t) {
    using type_category_type     = std::decay_t<decltype(t)>;
    constexpr auto type_category = nth::type<type_category_type>;
    constexpr size_t CategoryIndex =
        semantic_analysis::TypeSystem::index<type_category_type>();

    if constexpr (type_category == nth::type<core::ParameterType>) {
      auto& parameters = *proto.add_parameters();
      for (auto const& parameter : t.value()) {
        auto& param = *parameters.add_parameters();
        param.set_name(parameter.name);
        SerializeType(
            parameter.value, *param.mutable_type(),
            type_system.has_inline_storage(parameter.value.category()));
      }
    } else if constexpr (type_category == nth::type<core::PointerType>) {
      SerializeType(t.pointee(), *proto.add_pointers(),
                    type_system.has_inline_storage(t.pointee().category()));
    } else if constexpr (type_category ==
                         nth::type<semantic_analysis::BufferPointerType>) {
      SerializeType(t.pointee(), *proto.add_buffer_pointers(),
                    type_system.has_inline_storage(t.pointee().category()));
    } else if constexpr (type_category ==
                         nth::type<semantic_analysis::ArrayType>) {
      auto& array = *proto.add_arrays();
      array.set_length(t.length());
      SerializeType(t.data_type(), *array.mutable_type(),
                    type_system.has_inline_storage(t.data_type().category()));
    } else if constexpr (type_category ==
                         nth::type<semantic_analysis::SliceType>) {
      SerializeType(t.pointee(), *proto.add_slices(),
                    type_system.has_inline_storage(t.pointee().category()));
    } else if constexpr (type_category == nth::type<core::FunctionType>) {
      auto& f = *proto.add_functions();
      f.set_parameters(t.parameter_type().index());
      f.mutable_return_types()->Reserve(t.returns().size());
      for (auto return_type : t.returns()) {
        SerializeType(return_type, *f.add_return_types(),
                      type_system.has_inline_storage(return_type.category()));
      }
    } else if constexpr (type_category ==
                         nth::type<semantic_analysis::EnumType>) {
      auto& enumerators = *proto.add_enums()->mutable_enumerator();
      for (auto const& [identifier, value] : t.enumerators()) {
        enumerators[identifier] = value;
      }
    } else {
      static_assert(type_category.dependent(false));
    }
  });
}

void DeserializeTypeSystem(serialization::TypeSystem const& proto,
                           semantic_analysis::TypeSystem& type_system) {
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

  for (auto const& e : proto.enums()) {
    std::vector<std::pair<std::string, uint64_t>> enumerators;
    for (auto const& [identifier, value] : e.enumerator()) {
      enumerators.emplace_back(identifier, value);
    }
    semantic_analysis::EnumType(type_system, std::move(enumerators));
  }
}

}  // namespace

bool Module::Serialize(std::ostream& output,
                       GlobalFunctionMap& function_map) const {
  serialization::Module proto;

  *proto.mutable_identifier() = id_.value();
  SerializeTypeSystem(type_system(), *proto.mutable_type_system());

  GlobalModuleMap unused;
  // TODO: Fix const-correctness in Jasmin.
  vm::SerializationState state(const_cast<Module&>(*this).read_only_data(),
                               const_cast<Module&>(*this).foreign_symbol_map(),
                               serialization::ModuleIndex::Invalid(), unused,
                               function_map);

  serialization::ForeignSymbolMap::Serialize(foreign_symbol_map_,
                                             *proto.mutable_foreign_symbols());

  vm::Serialize(initializer_, *proto.mutable_initializer(), state);

  vm::Serialize(function_table_, *proto.mutable_function_table(), state);

  serialization::ReadOnlyData::Serialize(read_only_data_,
                                         *proto.mutable_read_only());
  data_types::Serialize(integer_table_, *proto.mutable_integers());

  auto& exported = *proto.mutable_exported();
  for (auto const& [name, symbols] : exported_symbols_) {
    auto& exported_symbols = exported[name];
    for (auto const& symbol : symbols) {
      auto& exported_symbol  = *exported_symbols.add_symbols();
      core::Type symbol_type = symbol.type();
      SerializeType(symbol_type, *exported_symbol.mutable_symbol_type(),
                    type_system().has_inline_storage(symbol_type.category()));
      if (symbol_type == semantic_analysis::Type) {
        SerializeType(symbol.as<core::Type>(), *exported_symbol.mutable_type(),
                      type_system().has_inline_storage(
                          symbol.as<core::Type>().category()));
      } else if (symbol_type.category() ==
                 type_system().index<core::FunctionType>()) {
        serialization::FunctionIndex::Serialize(
            symbol.as<TypedFunction>().function,
            *exported_symbol.mutable_function());
      } else {
        NOT_YET();
      }
    }
  }

  return proto.SerializeToOstream(&output);
}

bool Module::DeserializeInto(serialization::Module const& proto,
                             base::PtrSpan<Module const> dependencies,
                             serialization::ModuleIndex module_index,
                             Module& module, GlobalModuleMap& module_map,
                             GlobalFunctionMap& function_map) {
  data_types::Deserialize(proto.integers(), module.integer_table_);

  if (not serialization::ReadOnlyData::Deserialize(proto.read_only(),
                                                   module.read_only_data_) or
      not serialization::ForeignSymbolMap::Deserialize(
          proto.foreign_symbols(), module.foreign_symbol_map_)) {
    return false;
  }

  DeserializeTypeSystem(proto.type_system(), module.type_system_);

  for (auto const& [name, symbols] : proto.exported()) {
    for (auto const& symbol : symbols.symbols()) {
      core::Type symbol_type = DeserializeType(symbol.symbol_type());
      if (symbol_type == semantic_analysis::Type) {
        module.exported_symbols_[name].push_back(
            DeserializeType(symbol.type()));
      } else if (symbol_type.category() ==
                 module.type_system_.index<core::FunctionType>()) {
        serialization::FunctionIndex f;
        if (not serialization::FunctionIndex::Deserialize(symbol.function(),
                                                          f)) {
          return false;
        }
        module.exported_symbols_[name].push_back(TypedFunction{
            .type     = symbol_type,
            .function = f,
        });
      } else {
        NOT_YET(semantic_analysis::DebugType(symbol_type, module.type_system_));
      }
    }
  }

  vm::SerializationState state(module.read_only_data(),
                               module.foreign_symbol_map(), module_index,
                               module_map, function_map);
  if (not vm::Deserialize(proto.function_table(), module.function_table_,
                          module_index, state)) {
    return false;
  }
  vm::Deserialize(proto.initializer(), module.initializer_, state);

  return true;
}

std::pair<serialization::FunctionIndex, vm::Function const*> Module::Wrap(
    vm::Function const* f) {
  auto [iter, inserted] = wrappers_.try_emplace(f);
  if (inserted) {
    auto result =
        function_table().emplace(f->parameter_count(), f->return_count());
    auto [fn_index, fn_ptr] = result;
    fn_ptr->AppendPushFunction(f);
    fn_ptr->AppendCall();
    fn_ptr->AppendReturn();
    iter->second = result;
  }
  return iter->second;
}

}  // namespace module
