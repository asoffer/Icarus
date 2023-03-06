#ifndef ICARUS_VM_EXECUTE_H
#define ICARUS_VM_EXECUTE_H

#include "jasmin/execution_state.h"
#include "jasmin/value_stack.h"
#include "vm/function.h"
#include "vm/implementation.h"

namespace vm {

using ExecutionState =
    jasmin::ExecutionState<semantic_analysis::InstructionSet>;
void Execute(Function const& f, ExecutionState state,
             jasmin::ValueStack& value_stack);
void Execute(Function const& f, ExecutionState state,
             std::initializer_list<jasmin::Value> values);

void Execute(Function const& f, ExecutionState state,
             jasmin::ValueStack& value_stack,
             std::convertible_to<jasmin::Value> auto&... returns) {
  Execute(f, state, value_stack);
  int dummy;
  static_cast<void>(
      (dummy = ... =
           (returns = value_stack.pop<std::decay_t<decltype(returns)>>(), 0)));
}

void Execute(Function const& f, ExecutionState state,
             std::initializer_list<jasmin::Value> values,
             std::convertible_to<jasmin::Value> auto&... returns) {
  jasmin::ValueStack value_stack(values);
  Execute(f, state, value_stack, returns...);
}

}  // namespace vm

#endif  // ICARUS_VM_EXECUTE_H
