#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::UnaryOperator const *node,
                                      FunctionData data) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::BufferPointer: {
      Emit(node->operand(), data);
      data.function().AppendPush(&type_system());
      data.function().AppendMakeBufferPointerType();
    } break;
    case ast::UnaryOperator::Kind::Pointer: {
      Emit(node->operand(), data);
      data.function().AppendPush(&type_system());
      data.function().AppendMakePointerType();
    } break;
    case ast::UnaryOperator::Kind::Negate: {
      auto type = context().qualified_type(node).type();
      if (type == Integer) {
        Emit(node->operand(), data);
        data.function().AppendNegate<data_types::IntegerHandle>();
      } else {
        Emit(node->operand(), data);
        bool found = WithPrimitiveType(
            type, [&]<jasmin::Negatable T>() requires(std::is_signed_v<T>) {
              data.function().AppendNegate<T>();
            });
        if (not found) { NOT_YET(DebugType(type, type_system())); }
      }
    } break;
    case ast::UnaryOperator::Kind::At: {
      Emit(node->operand(), data);
      core::Bytes bytes_to_load =
          SizeOf(context().qualified_type(node).type(), type_system());
      data.function().AppendLoad(bytes_to_load.value());
    } break;
    case ast::UnaryOperator::Kind::Address: {
      as<ByteCodeReferenceEmitter>().Emit(node->operand(), data);
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
