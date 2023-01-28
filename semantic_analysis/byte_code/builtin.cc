#include "data_types/module_id.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Builtin const* node,
                                      FunctionData data) {
  data.function().append<jasmin::Push>(data_types::ModuleId::Builtin());
}

void ByteCodeStatementEmitter::operator()(ast::Builtin const*, FunctionData) {}

}  // namespace semantic_analysis
