#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::SliceType const* node,
                                      FunctionData data) {
  Emit(&node->data_type(), data);
  data.function().AppendPush(&GlobalTypeSystem);
  data.function().AppendMakeSliceType();
}

void ByteCodeStatementEmitter::operator()(ast::SliceType const* node,
                                          FunctionData data) {
  Emit(&node->data_type(), data);
}

}  // namespace semantic_analysis
