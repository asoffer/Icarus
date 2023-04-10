#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeStatementEmitter::operator()(ast::ReturnStmt const* node,
                                          FunctionData data) {
  std::span return_types = context()
                               .qualified_type(&node->function_literal())
                               .type()
                               .get<core::FunctionType>(type_system())
                               .returns();
  size_t i = 0;
  for (auto const* expr : node->exprs()) {
    as<ByteCodeValueEmitter>().CastTo(expr, QualifiedType(return_types[i]),
                                      data);
    ++i;
  }
  data.function().AppendReturn();
}

}  // namespace semantic_analysis
