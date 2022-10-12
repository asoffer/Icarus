#include "semantic_analysis/byte_code/byte_code.h"

#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

IrFunction EmitByteCode(QualifiedType qualified_type,
                        ast::Expression const &expression,
                        Context const &context, CompilerState &compiler_state) {
  IrFunction f =
      FitsInRegister(qualified_type.type(), compiler_state.type_system())
          ? IrFunction(0, 1)
          : IrFunction(1, 0);
  ByteCodeValueEmitter e(&context, compiler_state);
  e.EmitByteCode(&expression, f);
  f.append<jasmin::Return>();
  return f;
}

}  // namespace semantic_analysis
