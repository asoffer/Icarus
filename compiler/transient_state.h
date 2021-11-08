#ifndef ICARUS_COMPILER_TRANSIENT_STATE_H
#define ICARUS_COMPILER_TRANSIENT_STATE_H

#include <vector>

#include "ir/scope_state.h"

namespace compiler {

// Compiler state that needs to be tracked during the compilation of a single
// function or jump, but otherwise does not need to be saved.
struct TransientState {
  std::vector<ir::ScopeState> scope_landings;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_TRANSIENT_STATE_H
