#include "compiler/compiler.h"

#include "nth/container/flyweight_map.h"
#include "semantic_analysis/function_data.h"
#include "vm/function.h"

namespace compiler {

vm::Function Compiler::EvaluationFunction(Task &task) {
  vm::Function f(0, 1);

  // This `variable_offsets` map is intentionally empty. There will never be
  // declarations from which data needs to be loaded. Because `EvaluateAs` is
  // only to be called on constant expressions, any identifier will refer to a
  // declaration that is constant, and so lookup will happen by loading the
  // value directly rather than adding instructions which load at runtime.
  nth::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;
  task.send<FunctionData>(FunctionData(f, variable_offsets));
  task.complete();
  f.AppendReturn();
  return f;
}

}  // namespace compiler
