#include "type/serialize.h"

namespace ic::type {

void SerializeFunctionType(FunctionType type, FunctionTypeProto& proto) {
  auto& parameters = *proto.mutable_parameters();
  for (auto const& parameter : *type.parameters()) {
    Serialize(parameter.type, *parameters.Add()->mutable_type());
  }

  auto& returns = *proto.mutable_returns();
  for (Type t : type.returns()) { Serialize(t, *returns.Add()); }
}

void Serialize(Type type, TypeProto& proto) {
  proto.set_kind(
      static_cast<TypeProto::Kind>(static_cast<uint8_t>(type.kind()) + 1));
  proto.set_index(type.index());
}

void SerializeTypeSystem(TypeSystemProto& proto) {
  auto const& ts = GlobalTypeSystem();

  auto& fns = *proto.mutable_functions();
  fns.Reserve(ts.functions.size());
  for (size_t i = 0; i < ts.functions.size(); ++i) {
    SerializeFunctionType(FunctionType(i), *fns.Add());
  }

  auto& slices = *proto.mutable_slices();
  slices.Reserve(ts.slice_element_types.size());
  for (Type t : ts.slice_element_types) { Serialize(t, *slices.Add()); }
}

}  // namespace ic::type
