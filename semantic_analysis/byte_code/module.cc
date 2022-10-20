#include "ast/module.h"

#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::Module const* node, FunctionData data) {
  for (auto const* stmt : node->stmts()) { EmitByteCode(stmt, data); }
}

}  // namespace semantic_analysis

