#include "semantic_analysis/byte_code/emitter.h"
#include "type/primitive.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::Emit(ast::Terminal const* t, IrFunction& f) {
  if (context().qual_types(t)[0].type() == type::Bool) {
    f.append<jasmin::Push>(t->value().get<bool>());
  } else {
    NOT_YET();
  }
}

}  // namespace semantic_analysis
