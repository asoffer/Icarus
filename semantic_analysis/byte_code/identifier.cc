#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::Identifier const* node,
                                FunctionData data) {
  auto symbol = context().symbol(node);
  if (auto const* id = symbol.get_if<ast::Declaration::Id const>()) {
    auto qt = context().qualified_type(id);
    data.function().append<jasmin::StackOffset>(data.OffsetFor(id));
    core::Bytes bytes_to_load = SizeOf(qt.type(), type_system());
    data.function().append<jasmin::Load>(bytes_to_load.value());
  }
}


}  // namespace semantic_analysis
