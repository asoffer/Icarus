#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::Identifier const* node,
                                FunctionData data) {
  auto symbol = context().symbol(node);
  if (auto const * id = symbol.get_if<ast::Declaration::Id const>()) {
    data.function().append<jasmin::StackOffset>(data.OffsetFor(id));
    auto qt = context().qualified_type(id);
    if (PassInRegister(qt, type_system())) {
      data.function().append<jasmin::Load>(
          SizeOf(qt.type(), type_system()).value());
    }
  }
}

}  // namespace semantic_analysis
