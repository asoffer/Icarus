#include "module/module.h"

#include "base/meta.h"
#include "jasmin/serialization.h"
#include "module/module.pb.h"

namespace module {
namespace {

core::Type DeserializeType(internal_proto::Type const& proto) {
  return core::Type(proto.category(),
                    proto.has_value() ? proto.value() : proto.index());
}

void SerializeType(core::Type t, internal_proto::Type& proto,
                   bool inline_storage) {
  proto.set_category(t.category());
  if (inline_storage) {
    proto.set_value(t.index());
  } else {
    proto.set_index(t.index());
  }
}

void SerializeTypeSystem(semantic_analysis::TypeSystem& type_system,
                         internal_proto::TypeSystem& proto) {
  type_system.visit_all_stored([&](auto t) {
    using type_category_type     = std::decay_t<decltype(t)>;
    constexpr auto type_category = base::meta<type_category_type>;
    constexpr size_t CategoryIndex =
        semantic_analysis::TypeSystem::index<type_category_type>();

    if constexpr (type_category == base::meta<core::ParameterType>) {
      auto& parameters = *proto.add_parameters();
      for (auto const& parameter : t.value()) {
        auto& param = *parameters.add_parameters();
        param.set_name(parameter.name);
        SerializeType(
            parameter.value, *param.mutable_type(),
            type_system.has_inline_storage(parameter.value.category()));
      }
    } else if constexpr (type_category == base::meta<core::PointerType>) {
      SerializeType(t.pointee(), *proto.add_pointers(),
                    type_system.has_inline_storage(t.pointee().category()));
    } else if constexpr (type_category ==
                         base::meta<semantic_analysis::BufferPointerType>) {
      SerializeType(t.pointee(), *proto.add_buffer_pointers(),
                    type_system.has_inline_storage(t.pointee().category()));
    } else if constexpr (type_category ==
                         base::meta<semantic_analysis::ArrayType>) {
      auto& array = *proto.add_arrays();
      array.set_length(t.length());
      SerializeType(t.data_type(), *array.mutable_type(),
                    type_system.has_inline_storage(t.data_type().category()));
    } else if constexpr (type_category ==
                         base::meta<semantic_analysis::SliceType>) {
      SerializeType(t.pointee(), *proto.add_slices(),
                    type_system.has_inline_storage(t.pointee().category()));
    } else if constexpr (type_category == base::meta<core::FunctionType>) {
      auto& f = *proto.add_functions();
      f.set_parameters(t.parameter_type().index());
      f.mutable_return_types()->Reserve(t.returns().size());
      for (auto return_type : t.returns()) {
        SerializeType(return_type, *f.add_return_types(),
                      type_system.has_inline_storage(return_type.category()));
      }
    } else {
      static_assert(base::always_false(type_category));
    }
  });
}

void DeserializeTypeSystem(internal_proto::TypeSystem const& proto,
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
}

void SerializeForeignSymbols(semantic_analysis::TypeSystem& type_system,
                             semantic_analysis::ForeignFunctionMap const& map,
                             internal_proto::ForeignSymbol& proto) {
  for (auto const& [key, value] : map) {
    auto const & [name, type ] = key;
    proto.set_name(name);
    SerializeType(type, *proto.mutable_type(),
                  type_system.has_inline_storage(type.category()));
  }
}

void DeserializeForeignSymbols(
    semantic_analysis::TypeSystem& type_system,
    google::protobuf::RepeatedPtrField<internal_proto::ForeignSymbol> const&
        proto,
    semantic_analysis::ForeignFunctionMap& map) {
  for (auto const & symbol : proto) {
    map.ForeignFunction(symbol.name(), DeserializeType(symbol.type()));
  }
}

void SerializeFunction(semantic_analysis::IrFunction const& f,
                       internal_proto::Function& proto) {
  proto.set_parameters(f.parameter_count());
  proto.set_returns(f.return_count());
  jasmin::Serialize(f, *proto.mutable_content());
}

semantic_analysis::IrFunction DeserializeFunction(
    internal_proto::Function const& proto) {
  semantic_analysis::IrFunction f(proto.parameters(), proto.returns());

  jasmin::Deserialize(proto.content(), f);
  return f;
}

}  // namespace

bool Module::Serialize(std::ostream& output) const {
  internal_proto::Module proto;

  SerializeTypeSystem(type_system(), *proto.mutable_type_system());

  SerializeForeignSymbols(type_system(), foreign_function_map(),
                          *proto.add_foreign_symbols());

  SerializeFunction(initializer_, *proto.mutable_initializer());
  proto.mutable_functions()->Reserve(functions_.size());
  for (auto const& function : functions_) {
    SerializeFunction(function, *proto.add_functions());
  }

  return proto.SerializeToOstream(&output);
}

std::optional<Module> Module::Deserialize(std::istream& input) {
  std::optional<Module> m;
  internal_proto::Module proto;
  if (not proto.ParseFromIstream(&input)) { return m; }
  m.emplace();

  DeserializeTypeSystem(proto.type_system(), m->type_system_);

  DeserializeForeignSymbols(m->type_system(), *proto.mutable_foreign_symbols(),
                            m->foreign_function_map_);

  m->initializer_ = DeserializeFunction(proto.initializer());
  for (auto const& function : proto.functions()) {
    m->functions_.push_back(DeserializeFunction(function));
  }
  return m;
}

}  // namespace module
