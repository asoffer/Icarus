#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Assignment const* node,
                                      FunctionData data) {
  this->as<ByteCodeStatementEmitter>().Emit(node, data);
}

void ByteCodeStatementEmitter::operator()(ast::Assignment const* node,
                                          FunctionData data) {
  base::PtrSpan rhs_span = node->rhs();
  for (auto iter = rhs_span.rbegin(); iter != rhs_span.rend(); ++iter) {
    as<ByteCodeValueEmitter>().Emit(*iter, data);
  }

  auto& f = data.function();
  for (auto const * l : node->lhs()) {
    as<ByteCodeReferenceEmitter>().Emit(l, data);
    f.append<jasmin::Swap>();
    auto t = context().qualified_type(l).type();
    f.append<jasmin::Store>(SizeOf(t, type_system()).value());
  }
}

}  // namespace semantic_analysis
