#ifndef ICARUS_TYPE_SERIALIZE_H
#define ICARUS_TYPE_SERIALIZE_H

#include <string>

#include "absl/types/span.h"
#include "ir/value/result_buffer.h"
#include "type/type.h"

namespace type {

// Serializes the value of type `t` held in `ref`  into `out`.
void SerializeValue(Type t, ir::CompleteResultRef ref, std::string& out);

// Deserializes the value of type `t` serialized in `span`, writing it into
// `buffer`. If deserialization succeeds, the number of bytes read is returned.
// If deserialization fails, a negative value is returned.
ssize_t DeserializeValue(Type t, absl::Span<std::byte const> span,
                         ir::CompleteResultBuffer& buffer);

}  // namespace type

#endif  // ICARUS_TYPE_SERIALIZE_H
