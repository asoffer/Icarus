#ifndef ICARUS_CORE_TYPE_SYSTEM_POINTER_H
#define ICARUS_CORE_TYPE_SYSTEM_POINTER_H

#include "core/type_system/type_system.h"

namespace core {

struct PointerType : TypeCategory<PointerType, Type> {
  explicit PointerType(TypeSystemSupporting<PointerType> auto& s, Type t)
      : TypeCategory(s, t) {}

  Type pointee() const { return std::get<0>(decompose()); }
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_POINTER_H
