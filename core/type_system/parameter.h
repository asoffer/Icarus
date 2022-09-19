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

  struct Begin : jasmin::StackMachineInstruction<Begin> {
    static constexpr void execute(jasmin::ValueStack& value_stack) {
      value_stack.push(new core::Parameters<Type>);
    }
  };

  struct Append : jasmin::StackMachineInstruction<Append> {
    static constexpr void execute(jasmin::ValueStack& value_stack, Type t) {
      value_stack.peek<core::Parameters<Type>*>()->append("", t);
    }
  };

  template <TypeSystemSupporting<ParameterType> TS>
  struct End : jasmin::StackMachineInstruction<End<TS>> {
    static constexpr Type execute(jasmin::ValueStack& value_stack, TS* system) {
      return ParameterType(*system,
                           std::move(*std::unique_ptr<core::Parameters<Type>>(
                               value_stack.pop<core::Parameters<Type>*>())));
    }
  };
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_PARAMETER_H
