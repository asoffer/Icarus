#include "semantic_analysis/byte_code/emitter.h"
#include "serialization/module_index.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Access const* node,
                                      FunctionData data) {
  QualifiedType operand_qt = context().qualified_type(node->operand());
  if (operand_qt.type().get_if<SliceType>(type_system())) {
    Emit(node->operand(), data);

    if (node->member_name() == "length") {
      data.function().append<jasmin::Swap>();
    }

    data.function().append<jasmin::Drop>(1);
  } else if (operand_qt.type() == Module) {
    auto& m = resources().module(
        EvaluateAs<serialization::ModuleIndex>(node->operand()));
    std::span symbols = m.LoadSymbols(node->member_name());
    switch (symbols.size()) {
      case 0: {
        NOT_YET();
      } break;
      case 1: {
        // TODO: What if it's a different kind of symbol?
        core::Type t = resources().Translate(symbols[0].as<core::Type>(),
                                             m.type_system(), type_system());
        data.function().append<jasmin::Push>(t);
      } break;
      default: {
        NOT_YET();
      } break;
    }
  } else {
    NOT_YET();
  }
}

void ByteCodeStatementEmitter::operator()(ast::Access const* node,
                                          FunctionData data) {
  Emit(node->operand(), data);
}

}  // namespace semantic_analysis
