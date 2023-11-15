#ifndef ICARUS_TYPE_DESERIALIZE_H
#define ICARUS_TYPE_DESERIALIZE_H

#include "type/type.h"
#include "type/type_system.pb.h"

namespace ic::type {

Type Deserialize(TypeProto const& proto, TypeSystemProto const& ts);

FunctionType DeserializeFunctionType(FunctionTypeProto const& proto,
                                     TypeSystemProto const& ts);

void DeserializeTypeSystem(TypeSystemProto const& proto);

}  // namespace ic

#endif  // ICARUS_TYPE_DESERIALIZE_H
