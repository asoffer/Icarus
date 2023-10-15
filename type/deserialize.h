#ifndef ICARUS_TYPE_DESERIALIZE_H
#define ICARUS_TYPE_DESERIALIZE_H

#include "type/type.h"
#include "type/type_system.pb.h"

namespace ic::type {

Type Deserialize(TypeProto const& proto);

}  // namespace ic

#endif  // ICARUS_TYPE_DESERIALIZE_H
