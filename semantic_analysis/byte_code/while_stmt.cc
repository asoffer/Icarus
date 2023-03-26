#include "jasmin/op_code.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::WhileStmt const* node,
                                      FunctionData data) {
  auto& f = data.function();
  jasmin::OpCodeRange condition_block(f.raw_instructions().size(), 0);
  Emit(&node->condition(), data);
  f.AppendNot();
  jasmin::OpCodeRange branch = f.AppendJumpIfWithPlaceholders();
  for (auto const* stmt : node->body()) {
    as<ByteCodeStatementEmitter>().Emit(stmt, data);
  }
  jasmin::OpCodeRange loop = f.AppendJumpWithPlaceholders();
  f.set_value(loop, 0, jasmin::OpCodeRange::Distance(condition_block, loop));
  jasmin::OpCodeRange landing_block(f.raw_instructions().size(), 0);
  f.set_value(branch, 0, jasmin::OpCodeRange::Distance(landing_block, branch));
}

void ByteCodeStatementEmitter::operator()(ast::WhileStmt const* node,
                                          FunctionData data) {
  this->as<ByteCodeValueEmitter>().Emit(node, data);
}

}  // namespace semantic_analysis
