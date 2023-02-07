#include "jasmin/op_code.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Import const* node,
                                      FunctionData data) {
  data.function().append<jasmin::Push>(
      context().constant<module::ModuleIndex>(node));
}

void ByteCodeStatementEmitter::operator()(ast::Import const* node,
                                          FunctionData data) {
  // TODO: Warning. An ignored import statement is never correct. Or try to do
  // this during type-checking.
}

}  // namespace semantic_analysis
