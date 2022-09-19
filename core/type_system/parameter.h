#ifndef ICARUS_CORE_TYPE_SYSTEM_PARAMETER_H
#define ICARUS_CORE_TYPE_SYSTEM_PARAMETER_H

#include <memory>
#include <utility>

#include "core/parameters.h"
#include "core/type_system/type_system.h"

namespace core {

struct ParameterType : TypeCategory<ParameterType, core::Parameters<Type>> {
  explicit ParameterType(TypeSystemSupporting<ParameterType> auto& s,
                         core::Parameters<Type> parameters)
      : TypeCategory(s, std::move(parameters)) {}

  core::Parameters<Type> const& value() const {
    return std::get<0>(decompose());
  }
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_PARAMETER_H
