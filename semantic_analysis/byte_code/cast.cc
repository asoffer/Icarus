#include "ir/value/module_id.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Cast const* node,
                                      FunctionData data) {
  QualifiedType from_qt = context().qualified_type(node->expr());
  QualifiedType to_qt   = context().qualified_type(node);
  if (from_qt.type() == to_qt.type()) { return; }
  if (from_qt.type() == Integer) {
    std::span<std::byte const> evaluation =
        EvaluateConstant(node->expr(), from_qt);
    ASSERT(sizeof(ir::Integer*) == evaluation.size());
    ir::Integer *i;
    std::memcpy(&i, evaluation.data(), sizeof(i));
    if (to_qt.type() == I(8)) {
      data.function().append<jasmin::Push>(i->as_type<int8_t>());
    } else if (to_qt.type() == I(16)) {
      data.function().append<jasmin::Push>(i->as_type<int16_t>());
    } else if (to_qt.type() == I(32)) {
      data.function().append<jasmin::Push>(i->as_type<int32_t>());
    } else if (to_qt.type() == I(64)) {
      data.function().append<jasmin::Push>(i->as_type<int64_t>());
    } else if (to_qt.type() == U(8)) {
      data.function().append<jasmin::Push>(i->as_type<uint8_t>());
    } else if (to_qt.type() == U(16)) {
      data.function().append<jasmin::Push>(i->as_type<uint16_t>());
    } else if (to_qt.type() == U(32)) {
      data.function().append<jasmin::Push>(i->as_type<uint32_t>());
    } else if (to_qt.type() == U(64)) {
      data.function().append<jasmin::Push>(i->as_type<uint64_t>());
    } else {
      NOT_YET();
    }
  } else {
    NOT_YET();
  }
}

void ByteCodeStatementEmitter::operator()(ast::Cast const* node,
                                          FunctionData data) {
  core::Type from_type = context().qualified_type(node->expr()).type();
  core::Type to_type   = context().qualified_type(node).type();
  if (IsNumeric(from_type) and IsNumeric(to_type)) {
    Emit(node->expr(), data);
  } else {
    as<ByteCodeValueEmitter>().Emit(node->expr(), data);
    data.function().append<jasmin::Drop>(
        SizeOf(to_type, type_system()).value());
  }
}

}  // namespace semantic_analysis
