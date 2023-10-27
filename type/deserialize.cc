#include "type/deserialize.h"

#include "nth/debug/debug.h"
#include "type/type.h"

namespace ic::type {

ParametersType::Parameter DeserializeParameterType(
    ParameterTypeProto const& proto, TypeSystemProto const& ts) {
  return {
      // TODO: This number is wrong.
      .name = 0,
      .type = Deserialize(proto.type(), ts),
  };
}

FunctionType DeserializeFunctionType(FunctionTypeProto const& proto,
                                     TypeSystemProto const& ts) {
  std::vector<ParametersType::Parameter> parameters;
  parameters.reserve(proto.parameters().size());
  for (auto const& parameter_type : proto.parameters()) {
    parameters.push_back(DeserializeParameterType(parameter_type, ts));
  }

  std::vector<Type> returns;
  returns.reserve(proto.returns().size());
  for (auto const& return_type : proto.returns()) {
    returns.push_back(Deserialize(return_type, ts));
  }
  // TODO: Encode evaluation.
  return Function(Parameters(std::move(parameters)), std::move(returns),
                  Evaluation::PreferRuntime);
}

SliceType DeserializeSliceType(TypeProto const& proto,
                               TypeSystemProto const& ts) {
  return Slice(Deserialize(proto, ts));
}

PointerType DeserializePointerType(TypeProto const& proto,
                                   TypeSystemProto const& ts) {
  return Ptr(Deserialize(proto, ts));
}

BufferPointerType DeserializeBufferPointerType(TypeProto const& proto,
                               TypeSystemProto const& ts) {
  return BufPtr(Deserialize(proto, ts));
}

Type Deserialize(TypeProto const& proto, TypeSystemProto const& ts) {
  switch (proto.kind()) {
    case TypeProto::PRIMITIVE:
      return type::PrimitiveType(
          static_cast<type::PrimitiveType::Kind>(proto.index()));
    case TypeProto::FUNCTION:
      return DeserializeFunctionType(ts.functions(proto.index()), ts);
    case TypeProto::SLICE:
      return DeserializeSliceType(ts.slices(proto.index()), ts);
    case TypeProto::POINTER:
      return DeserializePointerType(ts.pointers(proto.index()), ts);
    case TypeProto::BUFFER_POINTER:
      return DeserializeBufferPointerType(ts.buffer_pointers(proto.index()), ts);
    case TypeProto::OPAQUE: return OpaqueType(proto.index());
    default: NTH_UNREACHABLE();
  }
}

void DeserializeTypeSystem(TypeSystemProto const& proto) {
  for (auto const& f : proto.functions()) { DeserializeFunctionType(f, proto); }
  for (auto const& t : proto.slices()) { DeserializeSliceType(t, proto); }
  for (auto const& t : proto.pointers()) { DeserializePointerType(t, proto); }
  for (auto const& t : proto.buffer_pointers()) {
    DeserializeBufferPointerType(t, proto);
  }
}

}  // namespace ic::type
