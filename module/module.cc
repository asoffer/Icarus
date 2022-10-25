#include "module/module.h"

#include "base/meta.h"
#include "jasmin/serialization.h"
#include "module/module.pb.h"

namespace module {
namespace {

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
        auto& param = *parameters.add_parameter();
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
      f.mutable_return_type()->Reserve(t.returns().size());
      for (auto return_type : t.returns()) {
        SerializeType(return_type, *f.add_return_type(),
                      type_system.has_inline_storage(return_type.category()));
      }
    } else {
      static_assert(base::always_false(type_category));
    }
  });
}

}  // namespace

bool Module::Serialize(std::ostream &output) const {
  internal_proto::Module proto;
  SerializeTypeSystem(type_system(), *proto.mutable_type_system());
  auto& initializer = *proto.mutable_initializer();
  initializer.set_parameters(initializer_.parameter_count());
  initializer.set_returns(initializer_.return_count());
  jasmin::Serialize(initializer_, *initializer.mutable_content());
  return proto.SerializeToOstream(&output);
}

std::optional<Module> Module::Deserialize(std::istream &input) {
  std::optional<Module> m;
  internal_proto::Module proto;
  if (not proto.ParseFromIstream(&input)) { return m; }
  m.emplace();

  jasmin::Deserialize(proto.initializer().content(), m->initializer_);
  return m;
}

}  // namespace module
