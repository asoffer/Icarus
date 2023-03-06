#include "vm/execute.h"

#include "jasmin/execute.h"
#include "jasmin/execution_state.h"
#include "vm/implementation.h"

namespace vm {

void Execute(Function const& f, ExecutionState state,
             jasmin::ValueStack& value_stack) {
  jasmin::Execute(internal::Impl(f.raw()), state, value_stack);
}

void Execute(Function const& f, ExecutionState state,
              std::initializer_list<jasmin::Value> values) {
  jasmin::ValueStack value_stack(values);
  Execute(f, state, value_stack);
}

}  // namespace vm
