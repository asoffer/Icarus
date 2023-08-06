#include "module/unique_id.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Access const* node,
                                      FunctionData data) {
  QualifiedType operand_qt = context().qualified_type(node->operand());
  if (operand_qt.type().get_if<SliceType>(type_system())) {
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
        core::Type symbol_type = resources().Translate(
            symbols[0].type(), module_id, m.type_system(), type_system());
        if (auto fn_type =
                symbol_type.get_if<core::FunctionType>(type_system())) {
          NTH_UNIMPLEMENTED();
        } else if (symbol_type == Type) {
          core::Type t =
              resources().Translate(symbols[0].as<core::Type>(), module_id,
                                    m.type_system(), type_system());
          data.function().AppendPush(t);
        } else if (symbol_type.category() ==
                   type_system().index<PrimitiveType>()) {
          // TODO: There's no need for translation here, but do we want to call
          // a translation for consistency?
          data.function().AppendPush(symbols[0].as<module::TypedValue>().value);
        } else {
          NTH_UNIMPLEMENTED();
        }
      } break;
      default: {
        NTH_UNIMPLEMENTED();
      } break;
    }
  } else if (operand_qt.type() == Type) {
    NTH_ASSERT(operand_qt.qualifiers() >= Qualifiers::Constant());
    core::Type t = EvaluateAs<core::Type>(node->operand());
    if (auto e = t.get_if<EnumType>(type_system())) {
      auto o = e->value(node->member_name());
      NTH_ASSERT(o.has_value());
      data.function().AppendPush(*o);
    } else {
      NTH_UNIMPLEMENTED();
    }
  } else {
    NTH_UNIMPLEMENTED("{}: {}") <<=
        {node->DebugString(), DebugType(operand_qt.type(), type_system())};
  }
}

void ByteCodeStatementEmitter::operator()(ast::Access const* node,
                                          FunctionData data) {
  Emit(node->operand(), data);
}

}  // namespace semantic_analysis
