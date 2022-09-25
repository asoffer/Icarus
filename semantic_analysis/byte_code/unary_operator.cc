#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

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
    default: NOT_YET();
  }
}

}  // namespace semantic_analysis
