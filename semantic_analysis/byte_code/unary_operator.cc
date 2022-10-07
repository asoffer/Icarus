#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

template <typename F>
bool WithPrimitiveType(core::Type t, F &&f) {
  if (t == I(8)) {
    std::forward<F>(f).template operator()<int8_t>();
    return true;
  } else if (t == I(16)) {
    std::forward<F>(f).template operator()<int16_t>();
    return true;
  } else if (t == I(32)) {
    std::forward<F>(f).template operator()<int32_t>();
    return true;
  } else if (t == I(64)) {
    std::forward<F>(f).template operator()<int64_t>();
    return true;
  } else if (t == U(8)) {
    std::forward<F>(f).template operator()<uint8_t>();
    return true;
  } else if (t == U(16)) {
    std::forward<F>(f).template operator()<uint16_t>();
    return true;
  } else if (t == U(32)) {
    std::forward<F>(f).template operator()<uint32_t>();
    return true;
  } else if (t == U(64)) {
    std::forward<F>(f).template operator()<uint64_t>();
    return true;
  } else if (t == F32) {
    std::forward<F>(f).template operator()<float>();
    return true;
  } else if (t == F64) {
    std::forward<F>(f).template operator()<double>();
    return true;
  } else {
    return false;
  }
}

void ByteCodeValueEmitter::Emit(ast::UnaryOperator const *node, IrFunction &f) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::BufferPointer: {
      EmitByteCode(node->operand(), f);
      f.append<jasmin::Push>(&type_system());
      f.append<TypeSystem::Make<BufferPointerType>>();
    } break;
    case ast::UnaryOperator::Kind::Pointer: {
      EmitByteCode(node->operand(), f);
      f.append<jasmin::Push>(&type_system());
      f.append<TypeSystem::Make<core::PointerType>>();
    } break;
    case ast::UnaryOperator::Kind::Negate: {
      auto type = context().qualified_type(node).type();
      EmitByteCode(node->operand(), f);
      bool found = WithPrimitiveType(
          type, [&]<jasmin::Negatable T> { f.append<jasmin::Negate<T>>(); });
      if (not found) { NOT_YET(); }
    } break;
    default: NOT_YET();
  }
}

}  // namespace semantic_analysis
