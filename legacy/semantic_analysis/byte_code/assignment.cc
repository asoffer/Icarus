#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::Assignment const* node,
                                      FunctionData data) {
  this->as<ByteCodeStatementEmitter>().Emit(node, data);
}

void ByteCodeStatementEmitter::operator()(ast::Assignment const* node,
                                          FunctionData data) {
  base::PtrSpan rhs_span = node->rhs();
  auto l_iter = node->lhs().rbegin();
  for (auto iter = rhs_span.rbegin(); iter != rhs_span.rend();
       ++iter, ++l_iter) {
    QualifiedType l_qt = context().qualified_type(*l_iter);
    as<ByteCodeValueEmitter>().CastTo(*iter, l_qt, data);
  }

  auto& f = data.function();
  for (auto const * l : node->lhs()) {
    as<ByteCodeReferenceEmitter>().Emit(l, data);
    f.AppendSwap();
    f.AppendStore(SizeOf(context().qualified_type(l).type()).value());
  }
}

}  // namespace semantic_analysis
