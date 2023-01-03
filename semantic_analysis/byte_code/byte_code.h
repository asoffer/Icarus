#ifndef ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_BYTE_CODE_H
#define ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_BYTE_CODE_H

#include <optional>

#include "ast/expression.h"
#include "ast/module.h"
#include "base/debug.h"
#include "jasmin/execute.h"
#include "module/module.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/instruction_set.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

void EmitByteCodeForModule(ast::Module const& ast_module, Context& context,
                           module::Module& module);

// Returns an `IrFunction` accepting no parameters and whose execution computes
// the value associated with `expression`.
IrFunction EmitByteCode(QualifiedType qualified_type,
                        ast::Expression const& expression, Context& context,
                        module::Module& module);

// Evaluates `expr` in the given `context` if possible, returning `std::nullopt`
// in the event of execution failure. Note that behavior is undefined if `expr`
// does not represent a value of type `T`. That is, specifying the wrong type
// parameter `T` is undefined behavior and not guaranteed to result in a
// returned value of `std::nullopt`.
template <typename T>
T EvaluateAs(Context& context, module::Module& module,
             ast::Expression const* expr) {
  auto qt        = context.qualified_type(expr);
  bool has_error = (qt.qualifiers() >= Qualifiers::Error());
  ASSERT(not has_error);

  IrFunction f = EmitByteCode(qt, *expr, context, module);

  T result;
  if (PassInRegister(qt, module.type_system())) {
    jasmin::Execute(f, {}, result);
  } else {
    IrFunction wrapper(0, 0);
    wrapper.append<jasmin::Push>(&result);
    wrapper.append<jasmin::Push>(&f);
    wrapper.append<jasmin::Call>();
    wrapper.append<jasmin::Return>();
    jasmin::Execute(f, {});
  }
  return result;
}

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_BYTE_CODE_BYTE_CODE_H
