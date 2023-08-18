#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Terminal const* node,
                                      FunctionData data) {
  QualifiedType qt = context().qualified_type(node);
  if (qt.type() == Bool) {
    data.function().AppendPush(node->value<bool>());
  } else if (qt.type() == SliceType(GlobalTypeSystem, Char)) {
    data.function().AppendPushStringLiteral(node->value<std::string>());
  } else if (qt.type() == Integer) {
    data.function().AppendPush(node->value<core::Integer>());
  } else if (qt.type() == F64) {
    // TODO: Long-term these won't be doubles, but rather a "rational" type that
    // is arbitrary-precision.
    data.function().AppendPush(node->value<double>());
  } else if (qt.type() == Char) {
    data.function().AppendPush(node->value<data_types::Char>());
  } else if (qt.type() == Type) {
    data.function().AppendPush(node->value<core::Type>());
  } else if (qt.type() == NullPtr) {
    data.function().AppendPush(data_types::Null());
  } else {
    NTH_UNIMPLEMENTED("{}") <<= {node->DebugString()};
  }
}

void ByteCodeStatementEmitter::operator()(ast::Terminal const*, FunctionData) {}

}  // namespace semantic_analysis
