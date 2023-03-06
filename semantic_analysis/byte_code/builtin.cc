#include "serialization/module_index.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Builtin const* node,
                                      FunctionData data) {
  data.function().AppendPush(serialization::ModuleIndex::Builtin());
}

void ByteCodeStatementEmitter::operator()(ast::Builtin const*, FunctionData) {}

}  // namespace semantic_analysis
