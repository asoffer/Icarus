#ifndef ICARUS_TYPE_SERIALIZE_H
#define ICARUS_TYPE_SERIALIZE_H

#include <string>

#include "absl/types/span.h"
#include "base/flyweight_map.h"
#include "ir/value/result_buffer.h"
#include "module/shared_context.h"
#include "precompiled/value.pb.h"
#include "type/function.h"
#include "type/system.h"
#include "type/type.h"

namespace type {

// Serializes the value of type `t` held in `ref`  into `out`.
void SerializeValue(TypeSystem const& system, Type t, ir::CompleteResultRef ref,
                    precompiled::Value& value);

ir::CompleteResultBuffer DeserializeValue(TypeSystem const& system,
                                          precompiled::Value const& value);

}  // namespace type

#endif  // ICARUS_TYPE_SERIALIZE_H
