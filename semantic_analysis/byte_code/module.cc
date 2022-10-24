#include "ast/module.h"

#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeStatementEmitter::operator()(ast::Module const* node,
                                          FunctionData data) {
  for (auto const* stmt : node->stmts()) { Emit(stmt, data); }
}

}  // namespace semantic_analysis

