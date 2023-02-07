#include "module/module.h"

#include "jasmin/serialization.h"
#include "module/module.pb.h"
#include "nth/meta/sequence.h"
#include "nth/meta/type.h"

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
    } else {
      static_assert(type_category.dependent(false));
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

absl::flat_hash_map<void (*)(), std::pair<size_t, core::FunctionType>>
SerializeForeignSymbols(
    semantic_analysis::TypeSystem& type_system,
    semantic_analysis::ForeignFunctionMap const& map,
    google::protobuf::RepeatedPtrField<internal_proto::ForeignSymbol>& proto) {
  absl::flat_hash_map<void (*)(), std::pair<size_t, core::FunctionType>>
      index_map;
  for (auto const& [key, value] : map) {
    auto const& [name, type]    = key;
    auto const& [ir_fn, fn_ptr] = value;
    auto& symbol                = *proto.Add();
    index_map.try_emplace(fn_ptr, index_map.size(), type);
    symbol.set_name(name);
    SerializeType(type, *symbol.mutable_type(),
                  type_system.has_inline_storage(type.category()));
  }
  return index_map;
}

void DeserializeForeignSymbols(
    semantic_analysis::TypeSystem& type_system,
    google::protobuf::RepeatedPtrField<internal_proto::ForeignSymbol> const&
        proto,
    semantic_analysis::ForeignFunctionMap& map) {
  for (auto const& symbol : proto) {
    map.ForeignFunction(
        symbol.name(),
        DeserializeType(symbol.type()).get<core::FunctionType>(type_system));
  }
}

struct SerializationState {
  template <typename T>
  T& get() {
    return std::get<T>(state_);
  }

 private:
  std::tuple<semantic_analysis::PushFunction::serialization_state,
             semantic_analysis::InvokeForeignFunction::serialization_state,
             semantic_analysis::PushStringLiteral::serialization_state>
      state_;
};

void SerializeFunction(semantic_analysis::IrFunction const& f,
                       internal_proto::Function& proto,
                       SerializationState& state) {
  proto.set_parameters(f.parameter_count());
  proto.set_returns(f.return_count());
  jasmin::Serialize(f, *proto.mutable_content(), state);
}

void SerializeReadOnlyData(
    internal_proto::ReadOnlyData& data,
    semantic_analysis::PushStringLiteral::serialization_state const& state) {
  for (std::string_view content : state) {
    *data.add_strings() = std::string(content);
  }
}

void DeserializeReadOnlyData(
    internal_proto::ReadOnlyData const& data,
    semantic_analysis::PushStringLiteral::serialization_state& state) {
  for (std::string_view content : data.strings()) { state.index(content); }
}

}  // namespace

bool Module::Serialize(std::ostream& output) const {
  internal_proto::Module proto;

  SerializeTypeSystem(type_system(), *proto.mutable_type_system());

  SerializationState state;

  auto& foreign_state =
      state
          .get<semantic_analysis::InvokeForeignFunction::serialization_state>();
  foreign_state.set_foreign_function_map(&foreign_function_map());
  foreign_state.set_type_system(type_system());
  foreign_state.set_map(SerializeForeignSymbols(
      type_system(), foreign_function_map(), *proto.mutable_foreign_symbols()));

  SerializeFunction(initializer_, *proto.mutable_initializer(), state);
  proto.mutable_functions()->Reserve(functions_.size());

  auto& f_state =
      state.get<semantic_analysis::PushFunction::serialization_state>();

  for (auto& function : functions_) { f_state.index(&function); }
  size_t serialized_up_to = 0;
  // TODO look up iterator invalidation constraints to see if we can process
  // more than one at a time.
  while (serialized_up_to != f_state.size()) {
    auto const& f = **(f_state.begin() + serialized_up_to);
    SerializeFunction(f, *proto.add_functions(), state);
    ++serialized_up_to;
  }

  SerializeReadOnlyData(
      *proto.mutable_read_only(),
      state.get<semantic_analysis::PushStringLiteral::serialization_state>());
  data_types::Serialize(integer_table_, *proto.mutable_integers());

  return proto.SerializeToOstream(&output);
}

std::optional<Module> Module::Deserialize(std::istream& input) {
  std::optional<Module> m;
  internal_proto::Module proto;
  if (not proto.ParseFromIstream(&input)) { return m; }
  m.emplace(std::move(*proto.mutable_read_only()));

  SerializationState state;

  data_types::Deserialize(proto.integers(), m->integer_table_);
  DeserializeReadOnlyData(
      m->read_only_data(),
      state.get<semantic_analysis::PushStringLiteral::serialization_state>());

  DeserializeTypeSystem(proto.type_system(), m->type_system_);

  DeserializeForeignSymbols(m->type_system(), *proto.mutable_foreign_symbols(),
                            m->foreign_function_map_);

  // Populate the PushFunction state map.
  auto& f_state =
      state.get<semantic_analysis::PushFunction::serialization_state>();
  for (auto const& function : proto.functions()) {
    auto [id, f_ptr] =
        m->create_function(function.parameters(), function.returns());
    size_t index = f_state.index(f_ptr);
    ASSERT(id.local().value() == index);
  }

  auto& foreign_state =
      state
          .get<semantic_analysis::InvokeForeignFunction::serialization_state>();
  foreign_state.set_foreign_function_map(&m->foreign_function_map());
  foreign_state.set_type_system(m->type_system());

  jasmin::Deserialize(proto.initializer().content(), m->initializer_, state);
  size_t fn_index = 0;
  for (auto const& function : proto.functions()) {
    jasmin::Deserialize(function.content(), m->functions_[fn_index], state);
    ++fn_index;
  }

  return m;
}

}  // namespace module
