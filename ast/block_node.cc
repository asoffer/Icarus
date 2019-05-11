#include "ast/block_node.h"

#include <sstream>

namespace ast {
std::string BlockNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << name_->to_string(n) << " {\n"
     << stmts_.to_string(n + 1) << std::string(2 * n, ' ') << "} ";
  return ss.str();
}

ir::Results BlockNode::EmitIr(Context *ctx) {
  stmts_.EmitIr(ctx);
  block_scope_->MakeAllDestructions(ctx);
  return ir::Results{};
}

std::vector<ir::RegisterOr<ir::Addr>> BlockNode::EmitLVal(Context *) {
  UNREACHABLE();
}

}  // namespace ast
