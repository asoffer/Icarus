#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {
namespace {

void EmitNonconstantDeclaration(ByteCodeStatementEmitter& emitter,
                                ast::Declaration const* node,
                                ByteCodeStatementEmitter::FunctionData data) {
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
      // TODO: Improve this EmitInitialize should be it's own derived CRTP.
      emitter.EmitInitialize(node->init_val(), data);
    } break;
    default: NOT_YET(node->DebugString());
  }
}
std::span<std::byte const> EmitConstantDeclaration(
    ByteCodeStatementEmitter& emitter, ast::Declaration const* node,
    QualifiedType qt, ByteCodeStatementEmitter::FunctionData data) {
  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      NOT_YET();
    } break;
    case ast::Declaration::kInferred: {
      if (node->ids().size() != 1) { NOT_YET(); }
      std::span x =  emitter.EvaluateConstant(node->init_val(), qt);
      return x;
    } break;
    default: NOT_YET(node->DebugString());
  }
}

}  // namespace

void ByteCodeStatementEmitter::operator()(ast::Declaration const* node,
                                          FunctionData data) {
  if (node->flags() & ast::Declaration::f_IsConst) {
    auto qt = context().qualified_type(node);
    std::span<std::byte const> evaluation =
        EmitConstantDeclaration(*this, node, qt, data);

    // TODO: memcpy here is error-prone. Design a better API.
    if (node->hashtags.contains(data_types::Hashtag::Export)) {
      if (qt.type() == Type) {
        core::Type t;
        std::memcpy(&t, evaluation.data(), sizeof(t));
        module().Export(node->ids()[0].name(), t);
      } else if (qt.type().category() ==
                 type_system().index<core::FunctionType>()) {
        auto const* f =
            jasmin::Value::Load(evaluation.data(), evaluation.size())
                .as<semantic_analysis::IrFunction const*>();
        module().Export(node->ids()[0].name(), qt.type(), f);
      } else {
        NOT_YET();
      }
    }

  } else {
    EmitNonconstantDeclaration(*this, node, data);
  }
}

void ByteCodeStatementEmitter::operator()(ast::Declaration::Id const* node,
                                          FunctionData data) {
  auto const& declaration = node->declaration();
  if (declaration.ids().size() != 1) { NOT_YET(); }
  if (declaration.flags() & ast::Declaration::f_IsConst) {
    // TODO do we want to be checking the initial value?
    auto qt = context().qualified_type(declaration.init_val());
    std::span<std::byte const> evaluation =
        EmitConstantDeclaration(*this, &declaration, qt, data);
    if (evaluation.size() <= jasmin::ValueSize) {
      if (qt.type().is<core::FunctionType>(type_system())) {
        data.function().append<PushFunction>(
            jasmin::Value::Load(evaluation.data(), evaluation.size()));
      } else {
        data.function().append<jasmin::Push>(
            jasmin::Value::Load(evaluation.data(), evaluation.size()));
      }
    } else {
      if (auto st = qt.type().get_if<SliceType>(type_system())) {
        if (st->pointee() == Char) {
          std::string_view view =
              *reinterpret_cast<std::string_view const*>(evaluation.data());
          data.function().append<PushStringLiteral>(view.data(), view.size());
        } else {
          NOT_YET(node->DebugString());
        }
      } else {
        NOT_YET(node->DebugString());
      }
    }
  } else {
    LOG("", "%s", node->DebugString());
    NOT_YET();
  }
}

void ByteCodeValueEmitter::operator()(ast::Declaration::Id const* node,
                                      FunctionData data) {
  as<ByteCodeStatementEmitter>().Emit(node, data);
}

}  // namespace semantic_analysis
