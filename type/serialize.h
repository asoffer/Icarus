#ifndef ICARUS_TYPE_SERIALIZE_H
#define ICARUS_TYPE_SERIALIZE_H

#include "type/type.h"
#include "type/type_system.pb.h"

namespace ic::type {

void Serialize(Type type, TypeProto& proto);

void SerializeFunctionType(FunctionType type, FunctionTypeProto& proto);

void SerializeTypeSystem(TypeSystemProto& proto);

}  // namespace ic::type

#endif  // ICARUS_TYPE_SERIALIZE_H
