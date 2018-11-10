#include "ast/block_node.h"

#include <sstream>
#include "ir/val.h"

namespace AST {
std::string BlockNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << name_->to_string(n) << " {\n"
     << stmts_.to_string(n + 1) << std::string(2 * n, ' ') << "} ";
  return ss.str();
}

void BlockNode::assign_scope(Scope *scope) {
  scope_ = scope;
  name_->assign_scope(scope);
  block_scope_ = scope->add_child<ExecScope>();
  stmts_.assign_scope(block_scope_.get());
}

type::Type const *BlockNode::VerifyType(Context *) {
  // TODO
  return type::Block;
}
void BlockNode::Validate(Context *) { NOT_YET(); }

void BlockNode::ExtractJumps(JumpExprs *rets) const {
  stmts_.ExtractJumps(rets);
}

base::vector<IR::Val> BlockNode::EmitIR(Context *ctx) {
  stmts_.EmitIR(ctx);
  return {};
}
base::vector<IR::Register> BlockNode::EmitLVal(Context *) { UNREACHABLE(); }

}  // namespace AST
