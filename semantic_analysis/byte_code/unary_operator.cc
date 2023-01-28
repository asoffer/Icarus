#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::UnaryOperator const *node,
                                      FunctionData data) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::BufferPointer: {
      Emit(node->operand(), data);
      data.function().append<jasmin::Push>(&type_system());
      data.function().append<TypeSystem::Make<BufferPointerType>>();
    } break;
    case ast::UnaryOperator::Kind::Pointer: {
      Emit(node->operand(), data);
      data.function().append<jasmin::Push>(&type_system());
      data.function().append<TypeSystem::Make<core::PointerType>>();
    } break;
    case ast::UnaryOperator::Kind::Negate: {
      auto type = context().qualified_type(node).type();
      if (type == Integer) {
        Emit(node->operand(), data);
        data.function().append<data_types::IntegerHandle::Negate>();
      } else {
        Emit(node->operand(), data);
        bool found = WithPrimitiveType(type, [&]<jasmin::Negatable T> {
          data.function().append<jasmin::Negate<T>>();
        });
        if (not found) { NOT_YET(DebugType(type, type_system())); }
      }
    } break;
    case ast::UnaryOperator::Kind::At: {
      Emit(node->operand(), data);
      core::Bytes bytes_to_load =
          SizeOf(context().qualified_type(node).type(), type_system());
      data.function().append<jasmin::Load>(bytes_to_load.value());
    } break;
    default: NOT_YET(node->DebugString());
  }
}

void ByteCodeStatementEmitter::operator()(ast::UnaryOperator const *node,
                                          FunctionData data) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::At:
    case ast::UnaryOperator::Kind::Pointer:
    case ast::UnaryOperator::Kind::BufferPointer: {
      Emit(node->operand(), data);
    } break;
    case ast::UnaryOperator::Kind::Negate: {
      auto type = context().qualified_type(node).type();
      Emit(node->operand(), data);
      if (type != Integer) {
        bool found = WithPrimitiveType(type, [&]<jasmin::Negatable T> {});
        if (not found) { NOT_YET(DebugType(type, type_system())); }
      }
    } break;
    default: NOT_YET();
  }
}

}  // namespace semantic_analysis
