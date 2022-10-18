#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::ReturnStmt const* node,
                                FunctionData data) {
  for (auto const* expr : node->exprs()) { EmitByteCode(expr, data); }
  data.function().append<jasmin::Return>();
}

}  // namespace semantic_analysis
