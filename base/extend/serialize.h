#ifndef ICARUS_BASE_EXTEND_BASE_SERIALIZE_H
#define ICARUS_BASE_EXTEND_BASE_SERIALIZE_H

#include "base/extend.h"
#include "base/extend/equality.h"

namespace base {

template <typename T>
struct BaseSerializeExtension {
  template <typename D>
  void BaseDeserialize(D &d, T &t) {
    std::apply([&](auto &... fields) { Deserialize(d, fields...); },
               t.field_refs());
  }

  template <typename S>
  void BaseSerialize(S &s, T const &t) {
    std::apply([&](auto const &... fields) { Serialize(s, fields...); },
               t.field_refs());
  }
};

}  // namespace base

#endif  // ICARUS_BASE_EXTEND_BASE_SERIALIZE_H
