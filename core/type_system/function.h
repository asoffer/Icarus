#ifndef ICARUS_CORE_TYPE_SYSTEM_FUNCTION_H
#define ICARUS_CORE_TYPE_SYSTEM_FUNCTION_H

#include <memory>
#include <utility>

#include "absl/types/span.h"
#include "core/parameters.h"
#include "core/type_system/parameter.h"
#include "core/type_system/type_system.h"

namespace core {

struct FunctionType
    : TypeCategory<FunctionType, ParameterType, std::vector<Type>> {
  explicit FunctionType(TypeSystemSupporting<FunctionType> auto& s,
                        ParameterType parameters, std::vector<Type> returns)
      : TypeCategory(s, std::move(parameters), std::move(returns)) {}

  ParameterType parameter_type() const { return std::get<0>(decompose()); }

  Parameters<Type> const& parameters() const {
    return parameter_type().value();
  }

  absl::Span<Type const> returns() const { return std::get<1>(decompose()); }

  template <TypeSystemSupporting<FunctionType> TS>
  struct End : jasmin::StackMachineInstruction<End<TS>> {
    static void execute(jasmin::ValueStack& value_stack, TS* system,
                        size_t num_return_types) {
      std::vector<Type> return_types(num_return_types);
      while (num_return_types != 0) {
        return_types[--num_return_types] = value_stack.pop<Type>();
      }
      value_stack.push(static_cast<Type>(FunctionType(
          *system, value_stack.pop<Type>().get<ParameterType>(*system),
          std::move(return_types))));
    }
  };
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_FUNCTION_H
