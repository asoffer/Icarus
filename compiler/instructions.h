#ifndef ICARUS_COMPILER_INSTRUCTIONS_H
#define ICARUS_COMPILER_INSTRUCTIONS_H

#include "base/untyped_buffer.h"
#include "ir/compiled_fn.h"
#include "ir/interpreter/evaluation_result.h"
#include "ir/value/native_fn.h"

namespace compiler {

void InterpretAtCompileTime(ir::NativeFn f);
base::untyped_buffer EvaluateAtCompileTimeToBuffer(ir::CompiledFn&& f);
interpreter::EvaluationResult EvaluateAtCompileTime(ir::CompiledFn&& f);
void WriteByteCode(ir::CompiledFn &fn);

}  // namespace compiler

#endif  // ICARUS_COMPILER_INSTRUCTIONS_H
