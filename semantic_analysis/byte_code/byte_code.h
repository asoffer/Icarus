#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_BYTE_CODE_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_BYTE_CODE_H

#include <optional>

#include "ast/expression.h"
#include "base/debug.h"
#include "jasmin/execute.h"
#include "semantic_analysis/byte_code/instruction_set.h"
#include "semantic_analysis/compiler_state.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

// Returns an `IrFunction` accepting no parameters and whose execution computes
// the value associated with `expression`.
IrFunction EmitByteCode(ast::Expression const& expression,
                        Context const& context, CompilerState& compiler_state);

// Evaluates `expr` in the given `context` if possible, returning `std::nullopt`
// in the event of execution failure. Note that behavior is undefined if `expr`
// does not represent a value of type `T`. That is, specifying the wrong type
// parameter `T` is undefined behavior and not guaranteed to result in a
// returned value of `std::nullopt`.
template <typename T>
std::optional<T> EvaluateAs(Context& context, CompilerState& compiler_state,
                            ast::Expression const* expr) {
  auto qt        = context.qualified_type(expr);
  bool has_error = (qt.qualifiers() >= Qualifiers::Error());
  ASSERT(has_error == false);

  IrFunction f = EmitByteCode(*expr, context, compiler_state);
  T result;
  jasmin::Execute(f, {}, result);
  return result;
}

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_BYTE_CODE_H
