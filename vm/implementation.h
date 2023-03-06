#ifndef ICARUS_VM_IMPLEMENTATION_H
#define ICARUS_VM_IMPLEMENTATION_H

#include "semantic_analysis/type_system.h"
#include "vm/instructions.h"

namespace vm::internal {

using IrFunction = jasmin::Function<InstructionSet>;

inline IrFunction &Impl(char *p) { return *reinterpret_cast<IrFunction *>(p); }
inline IrFunction const &Impl(char const *p) {
  return *reinterpret_cast<IrFunction const *>(p);
}

using ExecutionState = jasmin::ExecutionState<InstructionSet>;

inline ExecutionState &ExecutionStateImpl(char *p) {
  return *reinterpret_cast<ExecutionState *>(p);
}
inline ExecutionState const &ExecutionStateImpl(char const *p) {
  return *reinterpret_cast<ExecutionState const *>(p);
}

}  // namespace vm::internal

#endif  // ICARUS_VM_IMPLEMENTATION_H
