#ifndef ICARUS_BASE_EXTEND_BASE_SERIALIZE_H
#define ICARUS_BASE_EXTEND_BASE_SERIALIZE_H

#include "base/extend.h"
#include "base/serialize.h"

namespace base {

template <typename T>
struct BaseSerializeExtension {
  template <base::Deserializer D>
  friend bool BaseDeserialize(D &d, T &t) {
    return std::apply(
        [&](auto &... fields) { return base::Deserialize(d, fields...); },
        t.field_refs());
  }

  template <base::Serializer S>
  friend void BaseSerialize(S &s, T const &t) {
    std::apply([&](auto const &... fields) { base::Serialize(s, fields...); },
               t.field_refs());
  }
};

}  // namespace base

#endif  // ICARUS_BASE_EXTEND_BASE_SERIALIZE_H
