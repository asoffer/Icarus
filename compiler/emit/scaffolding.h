#ifndef ICARUS_COMPILER_EMIT_SCAFFOLDING_H
#define ICARUS_COMPILER_EMIT_SCAFFOLDING_H

#include "ast/scope.h"
#include "base/debug.h"
#include "compiler/compilation_data.h"
#include "compiler/transient_state.h"
#include "ir/blocks/group.h"

namespace compiler {

struct ScaffoldingCleanup {
  explicit ScaffoldingCleanup(TransientState *state)
      : state_ref_(*ASSERT_NOT_NULL(state)) {}
  ~ScaffoldingCleanup() { state_ref_.scaffolding.pop_back(); }

 private:
  TransientState &state_ref_;
};

ScaffoldingCleanup EmitScaffolding(CompilationDataReference ref,
                                   ir::internal::BlockGroupBase &group,
                                   ast::Scope const &scope);

}  // namespace compiler

#endif  // ICARUS_COMPILER_EMIT_SCAFFOLDING_H
