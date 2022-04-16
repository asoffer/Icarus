#ifndef ICARUS_TYPE_SERIALIZE_H
#define ICARUS_TYPE_SERIALIZE_H

#include <string>

#include "absl/types/span.h"
#include "base/flyweight_map.h"
#include "ir/value/result_buffer.h"
#include "module/module.pb.h"
#include "module/shared_context.h"
#include "type/function.h"
#include "type/system.h"
#include "type/type.h"

namespace type {

// Serializes the value of type `t` held in `ref`  into `out`.
void SerializeValue(TypeSystem const& system, Type t, ir::CompleteResultRef ref,
                    std::string& out);

// Deserializes the value of type `t` serialized in `span`, writing it into
// `buffer`. If deserialization succeeds, the number of bytes read is returned.
// If deserialization fails, a negative value is returned.
ssize_t DeserializeValue(
    Type t, absl::Span<std::byte const> span, ir::CompleteResultBuffer& buffer,
    base::flyweight_map<std::pair<std::string, Function const*>, void (*)()>&
        foreign_fn_map,
    TypeSystem& system);

module_proto::TypeSystem SerializeTypeSystem(TypeSystem const& system);

void SerializeTypeSystem(TypeSystem const& system, std::string& out);
bool DeserializeTypeSystem(std::string_view& content,
                           module::SharedContext& context, TypeSystem& system);

}  // namespace type

#endif  // ICARUS_TYPE_SERIALIZE_H
