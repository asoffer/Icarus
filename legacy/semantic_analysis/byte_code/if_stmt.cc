#include "jasmin/op_code.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::IfStmt const* node,
                                      FunctionData data) {
  auto& f = data.function();
  Emit(&node->condition(), data);

  jasmin::OpCodeRange branch = f.AppendJumpIfWithPlaceholders();
  for (auto const* stmt : node->false_block()) {
    as<ByteCodeStatementEmitter>().Emit(stmt, data);
  }
  jasmin::OpCodeRange false_to_land = f.AppendJumpWithPlaceholders();

  // `true_land` needs to be computed here.
  jasmin::OpCodeRange true_land(f.raw_instructions().size(), 0);
  for (auto const* stmt : node->true_block()) {
    as<ByteCodeStatementEmitter>().Emit(stmt, data);
  }
  jasmin::OpCodeRange land(f.raw_instructions().size(), 0);
  f.set_value(false_to_land, 0,
              jasmin::OpCodeRange::Distance(land, false_to_land));
  f.set_value(branch, 0, jasmin::OpCodeRange::Distance(true_land, branch));
}

void ByteCodeStatementEmitter::operator()(ast::IfStmt const* node,
                                          FunctionData data) {
  this->as<ByteCodeValueEmitter>().Emit(node, data);
}

}  // namespace semantic_analysis
