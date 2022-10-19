#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {
namespace {

void EmitNonconstantDeclaration(ByteCodeValueEmitter& emitter,
                                ast::Declaration const* node,
                                ByteCodeValueEmitter::FunctionData data) {
  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      for (auto const& id : node->ids()) {
        data.function().append<jasmin::StackOffset>(data.OffsetFor(&id));
        emitter.EmitDefaultInitialize(
            emitter.context().qualified_type(&id).type(), data);
      }
    } break;
    case ast::Declaration::kInferred: {
      for (auto const& id : node->ids()) {
        data.function().append<jasmin::StackOffset>(data.OffsetFor(&id));
      }
      emitter.EmitInitialize(node->init_val(), data);
    } break;
    default: NOT_YET(node->DebugString());
  }
}
void EmitConstantDeclaration(ByteCodeValueEmitter& emitter,
                             ast::Declaration const* node,
                             ByteCodeValueEmitter::FunctionData data) {
  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      NOT_YET();
    } break;
    case ast::Declaration::kInferred: {
      if (node->ids().size() != 1) { NOT_YET(); }
      absl::Span<std::byte const> evaluation = emitter.EvaluateConstant(
          node->init_val(), emitter.context().qualified_type(node->init_val()));
      // TODO: Make this a public constant in Jasmin.
      if (evaluation.size() <= 8) {
        data.function().append<jasmin::Push>(
            jasmin::Value::Load(evaluation.data(), evaluation.size()));
      } else {
        NOT_YET();
      }
    } break;
    default: NOT_YET(node->DebugString());
  }
}

}  // namespace

void ByteCodeValueEmitter::Emit(ast::Declaration const* node, FunctionData data) {
  if (node->flags() & ast::Declaration::f_IsConst) {
    EmitConstantDeclaration(*this, node, data);
  } else {
    EmitNonconstantDeclaration(*this, node, data);
  }
}

void ByteCodeValueEmitter::Emit(ast::Declaration::Id const* node, FunctionData data) {
  if (node->declaration().ids().size() != 1) { NOT_YET(); }
  EmitByteCode(&node->declaration(), data);
}

}  // namespace semantic_analysis
