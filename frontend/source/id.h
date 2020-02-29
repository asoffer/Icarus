#ifndef ICARUS_FRONTEND_SOURCE_ID_H
#define ICARUS_FRONTEND_SOURCE_ID_H

#include <cstdint>

#include "base/meta.h"

namespace frontend {

struct SourceId {
  template <typename T>
  static SourceId Make(uintptr_t id) {
    SourceId src_id;
    src_id.type_id_ = base::meta<T>.id();
    src_id.id_      = id;
    return src_id;
  }

 private:
  uintptr_t type_id_;
  uintptr_t id_;
}

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_ID_H
