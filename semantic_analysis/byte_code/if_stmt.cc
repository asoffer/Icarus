#include "jasmin/op_code.h"
#include "semantic_analysis/byte_code/emitter.h"

namespace semantic_analysis {

void ByteCodeValueEmitter::operator()(ast::IfStmt const* node,
                                      FunctionData data) {
  auto& f = data.function();
  Emit(&node->condition(), data);
  jasmin::OpCodeRange branch = f.append_with_placeholders<jasmin::JumpIf>();
  for (auto const* stmt : node->false_block()) { Emit(stmt, data); }
  jasmin::OpCodeRange false_to_land = f.append_with_placeholders<jasmin::Jump>();
  for (auto const* stmt : node->true_block()) { Emit(stmt, data); }
  jasmin::OpCodeRange land(f.raw_instructions().size(), 0);
  f.set_value(false_to_land, 0,
              jasmin::OpCodeRange::Distance(land, false_to_land));
  f.set_value(branch, 0, jasmin::OpCodeRange::Distance(land, branch));
}

void ByteCodeStatementEmitter::operator()(ast::IfStmt const* node,
                                          FunctionData data) {
  this->as<ByteCodeValueEmitter>().Emit(node, data);
}

}  // namespace semantic_analysis
