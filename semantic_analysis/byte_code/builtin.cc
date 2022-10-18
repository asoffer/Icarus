#include "ir/value/module_id.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::Builtin const* node, FunctionData data) {
  data.function().append<jasmin::Push>(ir::ModuleId::Builtin());
}

}  // namespace semantic_analysis
