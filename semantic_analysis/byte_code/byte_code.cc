#include "semantic_analysis/byte_code/byte_code.h"

#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

IrFunction EmitByteCode(QualifiedType qualified_type,
                        ast::Expression const &expression, Context &context,
                        CompilerState &compiler_state) {
  IrFunction f = PassInRegister(qualified_type, compiler_state.type_system())
                     ? IrFunction(0, 1)
                     : IrFunction(1, 0);
  ByteCodeValueEmitter e(&context, compiler_state);
  base::flyweight_map<ast::Declaration::Id const *, size_t> variable_offsets;
  e.EmitByteCode(&expression,
                 ByteCodeValueEmitter::FunctionData(f, variable_offsets));
  f.append<jasmin::Return>();
  return f;
}

}  // namespace semantic_analysis
