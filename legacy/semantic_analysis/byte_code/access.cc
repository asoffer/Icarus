#include "module/unique_id.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Access const* node,
                                      FunctionData data) {
  QualifiedType operand_qt = context().qualified_type(node->operand());
  if (operand_qt.type().get_if<SliceType>(GlobalTypeSystem)) {
    Emit(node->operand(), data);

    if (node->member_name() == "length") { data.function().AppendSwap(); }

    data.function().AppendDrop(1);
  } else if (operand_qt.type() == Module) {
    auto module_id    = EvaluateAs<module::UniqueId>(node->operand());
    auto& m           = resources().module(module_id);
    std::span symbols = m.LoadSymbols(node->member_name());
    switch (symbols.size()) {
      case 0: {
        NTH_UNIMPLEMENTED();
      } break;
      case 1: {
        NTH_UNIMPLEMENTED();
      } break;
    }
  } else if (operand_qt.type() == Type) {
    NTH_ASSERT(operand_qt.qualifiers() >= Qualifiers::Constant());
    core::Type t = EvaluateAs<core::Type>(node->operand());
    if (auto e = t.get_if<EnumType>(GlobalTypeSystem)) {
      auto o = e->value(node->member_name());
      NTH_ASSERT(o.has_value());
      data.function().AppendPush(*o);
    } else {
      NTH_UNIMPLEMENTED();
    }
  } else {
    NTH_UNIMPLEMENTED("{}: {}") <<=
        {node->DebugString(), DebugType(operand_qt.type())};
  }
}

void ByteCodeStatementEmitter::operator()(ast::Access const* node,
                                          FunctionData data) {
  Emit(node->operand(), data);
}

}  // namespace semantic_analysis
