#ifndef ICARUS_COMPILER_INSTRUCTIONS_H
#define ICARUS_COMPILER_INSTRUCTIONS_H

#include "base/untyped_buffer.h"
#include "ir/interpreter/evaluation_result.h"
#include "ir/value/native_fn.h"

namespace compiler {

void InterpretAtCompileTime(ir::NativeFn f);
void InterpretAtCompileTime(ir::CompiledFn const &fn);
base::untyped_buffer EvaluateAtCompileTimeToBuffer(ir::NativeFn f);
interpreter::EvaluationResult EvaluateAtCompileTime(ir::NativeFn f);
base::untyped_buffer EmitByteCode(ir::CompiledFn const &fn);

}  // namespace compiler

#endif  // ICARUS_COMPILER_INSTRUCTIONS_H
