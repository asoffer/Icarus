#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Terminal const* node,
                                      FunctionData data) {
  QualifiedType qt = context().qualified_type(node);
  if (qt.type() == Bool) {
    data.function().AppendPush(node->value<bool>());
  } else if (qt.type() == SliceType(type_system(), Char)) {
    data.function().AppendPushStringLiteral(node->value<std::string>());
  } else if (qt.type() == Integer) {
    data.function().AppendPush(
        module().integer_table().insert(node->value<nth::Integer>()));
  } else if (qt.type() == F64) {
    // TODO: Long-term these won't be doubles, but rather a "rational" type that
    // is arbitrary-precision.
    data.function().AppendPush(node->value<double>());
  } else if (qt.type() == Type) {
    data.function().AppendPush(node->value<core::Type>());
  } else {
    NOT_YET(node->DebugString());
  }
}

void ByteCodeStatementEmitter::operator()(ast::Terminal const*, FunctionData) {}

}  // namespace semantic_analysis
