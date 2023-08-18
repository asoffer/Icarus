#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Identifier const* node,
                                      FunctionData data) {
  if (node->name() == "builtin") {
    data.function().AppendPush(module::UniqueId::Builtin());
    return;
  }

  auto symbol = context().symbol(node);
  if (auto const* id = symbol.get_if<ast::Declaration::Id const>()) {
    auto qt = context().qualified_type(id);
    if (qt.qualifiers() >= Qualifiers::Constant()) {
      std::span<std::byte const> evaluation = EvaluateConstant(id, qt);
      if (evaluation.size() <= jasmin::ValueSize) {
        data.function().AppendPush(
            jasmin::Value::Load(evaluation.data(), evaluation.size()));
      } else {
        if (auto st = qt.type().get_if<SliceType>(GlobalTypeSystem)) {
          if (st->pointee() == Char) {
            std::string_view view =
                *reinterpret_cast<std::string_view const*>(evaluation.data());
            data.function().AppendPushStringLiteral(view);
          } else {
            NTH_UNIMPLEMENTED("{}") <<= {node->DebugString()};
          }
        } else {
          NTH_UNIMPLEMENTED("{}") <<= {node->DebugString()};
        }
      }
    } else {
      data.function().AppendStackOffset(data.OffsetFor(id));
      core::Bytes bytes_to_load = SizeOf(qt.type());
      if (bytes_to_load.value() <= jasmin::ValueSize) {
        data.function().AppendLoad(bytes_to_load.value());
      } else if (bytes_to_load.value() <= 2 * jasmin::ValueSize) {
        data.function().AppendDuplicate();
        data.function().AppendLoad(jasmin::ValueSize);
        data.function().AppendSwap();
        data.function().AppendIncrementPointer(jasmin::ValueSize);
        data.function().AppendLoad(bytes_to_load.value() - jasmin::ValueSize);
      }
    }
  }
}

void ByteCodeStatementEmitter::operator()(ast::Identifier const*,
                                          FunctionData) {}

void ByteCodeReferenceEmitter::operator()(ast::Identifier const* node,
                                          FunctionData data) {
  auto symbol = context().symbol(node);
  if (auto const* id = symbol.get_if<ast::Declaration::Id const>()) {
    auto qt = context().qualified_type(id);
    if (qt.qualifiers() >= Qualifiers::Constant()) {
      NTH_UNIMPLEMENTED();
    } else {
      data.function().AppendStackOffset(data.OffsetFor(id));
    }
  }
}

}  // namespace semantic_analysis
