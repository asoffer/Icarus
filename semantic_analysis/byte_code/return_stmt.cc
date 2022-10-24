#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeStatementEmitter::operator()(ast::ReturnStmt const* node,
                                          FunctionData data) {
  for (auto const* expr : node->exprs()) {
    as<ByteCodeValueEmitter>().Emit(expr, data);
  }
  data.function().append<jasmin::Return>();
}

}  // namespace semantic_analysis
