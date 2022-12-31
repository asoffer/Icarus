#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Terminal const* node,
                                      FunctionData data) {
  QualifiedType qt = context().qualified_type(node);
  if (qt.type() == Bool) {
    data.function().append<jasmin::Push>(node->value<bool>());
  } else if (qt.type() == SliceType(type_system(), Char)) {
    std::string_view slice = node->value<std::string>();
    data.function().append<PushStringLiteral>(slice.data(), slice.size());
  } else if (qt.type() == Integer) {
    // TODO: Store the integer in some shared manager type.
    auto* i = new ir::Integer(node->value<ir::Integer>());
    data.function().append<Construct<ir::Integer>>(i);
  } else if (qt.type() == F64) {
    // TODO: Long-term these won't be doubles, but rather a "rational" type that
    // is arbitrary-precision.
    data.function().append<jasmin::Push>(node->value<double>());
  } else if (qt.type() == Type) {
    data.function().append<jasmin::Push>(node->value<core::Type>());
  } else {
    NOT_YET(node->DebugString());
  }
}

void ByteCodeStatementEmitter::operator()(ast::Terminal const*, FunctionData) {}

}  // namespace semantic_analysis
