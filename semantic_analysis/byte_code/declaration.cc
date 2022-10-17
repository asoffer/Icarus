#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::Declaration const* node, FunctionData data) {
  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      for (auto const& id : node->ids()) {
        data.function().append<jasmin::StackOffset>(data.OffsetFor(&id));
        EmitDefaultInitialize(context().qualified_type(&id).type(), data);
      }
      NOT_YET();
    }
    default: NOT_YET();
  }
}

}  // namespace semantic_analysis
