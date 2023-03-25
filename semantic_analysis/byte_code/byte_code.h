#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_BYTE_CODE_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_BYTE_CODE_H

#include <optional>

#include "ast/expression.h"
#include "ast/module.h"
#include "base/debug.h"
#include "jasmin/execute.h"
#include "module/module.h"
#include "module/resources.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/type_system.h"
#include "vm/execute.h"

namespace semantic_analysis {

void EmitByteCodeForModule(ast::Module const& ast_module, Context& context,
                           module::Resources& resources);

// Returns an `vm::Function` accepting no parameters and whose execution computes
// the value associated with `expression`.
vm::Function EmitByteCode(QualifiedType qualified_type,
                        ast::Expression const& expression, Context& context,
                        module::Resources& module);

// Evaluates `expr` in the given `context` if possible, returning `std::nullopt`
// in the event of execution failure. Note that behavior is undefined if `expr`
// does not represent a value of type `T`. That is, specifying the wrong type
// parameter `T` is undefined behavior and not guaranteed to result in a
// returned value of `std::nullopt`.
template <typename T>
T EvaluateAs(Context& context, module::Resources& resources,
             ast::Expression const* expr) {
  auto qt        = context.qualified_type(expr);
  bool has_error = (qt.qualifiers() >= Qualifiers::Error());
  ASSERT(not has_error);

  vm::Function f = EmitByteCode(qt, *expr, context, resources);

  data_types::IntegerTable table;
  vm::ArgumentSlice argument_slice(nullptr, 0);
  vm::ExecutionState state{table, resources.primary_module().type_system(),
                           argument_slice};
  T result;
  if (PassInRegister(qt, resources.primary_module().type_system())) {
    vm::Execute(f, state, {}, result);
  } else {
    vm::Function wrapper(0, 0);
    wrapper.AppendPush(&result);
    wrapper.AppendPush(&f);
    wrapper.AppendCall();
    wrapper.AppendReturn();
    vm::Execute(f, state, {});
  }

  return result;
}

std::span<std::byte const> EvaluateConstant(Context& context,
                                            module::Resources& resources,
                                            ast::Expression const* expr,
                                            QualifiedType qt);
}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_BYTE_CODE_H
