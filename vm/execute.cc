#include "vm/execute.h"

#include <utility>

#include "jasmin/execute.h"
#include "jasmin/execution_state.h"
#include "vm/implementation.h"

namespace vm {

static_assert(sizeof(internal::ExecutionState) == sizeof(ExecutionState));

ExecutionState::ExecutionState(data_types::IntegerTable& table) {
  new (data_) internal::ExecutionState(table);
}

ExecutionState::ExecutionState(ExecutionState const& state) {
  new (data_)
      internal::ExecutionState(internal::ExecutionStateImpl(state.data_));
}

ExecutionState::ExecutionState(ExecutionState&& state) {
  new (data_) internal::ExecutionState(
      std::move(internal::ExecutionStateImpl(state.data_)));
}

ExecutionState& ExecutionState::operator=(ExecutionState const& state) {
  internal::ExecutionStateImpl(data_) =
      internal::ExecutionStateImpl(state.data_);
  return *this;
}

ExecutionState& ExecutionState::operator=(ExecutionState&& state) {
  internal::ExecutionStateImpl(data_) =
      std::move(internal::ExecutionStateImpl(state.data_));
  return *this;
}

ExecutionState::~ExecutionState() {
  using ExecutionState = internal::ExecutionState;
  internal::ExecutionStateImpl(data_).~ExecutionState();
}

void Execute(Function const& f, ExecutionState state,
             jasmin::ValueStack& value_stack) {
  jasmin::Execute(internal::Impl(f.raw()),
                  internal::ExecutionStateImpl(state.data_), value_stack);
}

void Execute(Function const& f, ExecutionState state,
             std::initializer_list<jasmin::Value> values) {
  jasmin::ValueStack value_stack(values);
  Execute(f, state, value_stack);
}

}  // namespace vm
