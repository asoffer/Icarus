#include "semantic_analysis/byte_code/emitter.h"
#include "serialization/module_index.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Access const* node,
                                      FunctionData data) {
  QualifiedType operand_qt = context().qualified_type(node->operand());
  if (operand_qt.type().get_if<SliceType>(type_system())) {
    Emit(node->operand(), data);

    if (node->member_name() == "length") { data.function().AppendSwap(); }

    data.function().AppendDrop(1);
  } else if (operand_qt.type() == Module) {
    auto module_index = EvaluateAs<serialization::ModuleIndex>(node->operand());
    auto& m           = resources().module(module_index);
    std::span symbols = m.LoadSymbols(node->member_name());
    switch (symbols.size()) {
      case 0: {
        NOT_YET();
      } break;
      case 1: {
        core::Type symbol_type = resources().Translate(
            symbols[0].type(), module_index, m.type_system(), type_system());
        if (auto fn_type =
                symbol_type.get_if<core::FunctionType>(type_system())) {
          NOT_YET();
        } else if (symbol_type == Type) {
          core::Type t =
              resources().Translate(symbols[0].as<core::Type>(), module_index,
                                    m.type_system(), type_system());
          data.function().AppendPush(t);
        } else if (symbol_type.category() ==
                   type_system().index<PrimitiveType>()) {
          // TODO: There's no need for translation here, but do we want to call
          // a translation for consistency?
          data.function().AppendPush(symbols[0].as<module::TypedValue>().value);
        } else {
          NOT_YET();
        }
      } break;
      default: {
        NOT_YET();
      } break;
    }
  } else if (operand_qt.type() == Type) {
    ASSERT(operand_qt.qualifiers() >= Qualifiers::Constant());
    core::Type t = EvaluateAs<core::Type>(node->operand());
    if (auto e = t.get_if<EnumType>(type_system())) {
      auto o = e->value(node->member_name());
      ASSERT(o.has_value());
      data.function().AppendPush(*o);
    } else {
      NOT_YET();
    }
  } else {
    NOT_YET(node->DebugString(), DebugType(operand_qt.type(), type_system()));
  }
}

void ByteCodeStatementEmitter::operator()(ast::Access const* node,
                                          FunctionData data) {
  Emit(node->operand(), data);
}

}  // namespace semantic_analysis
