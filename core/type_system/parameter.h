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
    static void execute(jasmin::ValueStack& value_stack) {
      value_stack.push(new core::Parameters<Type>);
    }
  };

  struct Append : jasmin::StackMachineInstruction<Append> {
    static core::Parameters<Type>* execute(core::Parameters<Type>* parameters,
                                           Type t) {
      parameters->append("", t);
      return parameters;
    }
  };

  struct AppendNamed : jasmin::StackMachineInstruction<AppendNamed> {
    static core::Parameters<Type>* execute(core::Parameters<Type>* parameters,
                                           char const* buffer, size_t length,
                                           Type t) {
      parameters->append(std::string(buffer, length), t);
      return parameters;
    }
  };

  template <TypeSystemSupporting<ParameterType> TS>
  struct End : jasmin::StackMachineInstruction<End<TS>> {
    static void execute(jasmin::ValueStack& value_stack, TS* system) {
      std::unique_ptr<core::Parameters<Type>> ptr(
          value_stack.pop<core::Parameters<Type>*>());
      value_stack.push(
          static_cast<Type>(ParameterType(*system, std::move(*ptr))));
    }
  };
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_PARAMETER_H
