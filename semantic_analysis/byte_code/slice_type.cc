#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::SliceType const* node,
                                      FunctionData data) {
  Emit(&node->data_type(), data);
  data.function().append<jasmin::Push>(&type_system());
  data.function().append<TypeSystem::Make<SliceType>>();
}

void ByteCodeStatementEmitter::operator()(ast::SliceType const* node,
                                          FunctionData data) {
  Emit(&node->data_type(), data);
}

}  // namespace semantic_analysis
