#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {
namespace {

template <typename T>
concept Negatable = jasmin::Negatable<T> and not std::same_as<T, bool>;

}  // namespace

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
          type, [&]<Negatable T> { f.append<jasmin::Negate<T>>(); });
      if (not found) { NOT_YET(DebugType(type, type_system())); }
    } break;
    default: NOT_YET();
  }
}

}  // namespace semantic_analysis
