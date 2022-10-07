#include "semantic_analysis/byte_code/byte_code.h"

#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

IrFunction EmitByteCode(ast::Expression const &expression,
                        Context const &context, CompilerState &compiler_state) {
  IrFunction f(0, 1);
  ByteCodeValueEmitter e(&context, compiler_state);
  e.EmitByteCode(&expression, f);
  f.append<jasmin::Return>();
  return f;
}

}  // namespace semantic_analysis
