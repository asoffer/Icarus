#include "ast/block_node.h"

#include <sstream>
#include "ir/val.h"

namespace ast {
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

void BlockNode::DependentDecls(base::Graph<Declaration *> *g,
                               Declaration *d) const {
  name_->DependentDecls(g, d);
  stmts_.DependentDecls(g, d);
}

VerifyResult BlockNode::VerifyType(Context *ctx) {
  stmts_.VerifyType(ctx);
  return VerifyResult::Constant(type::Block);
}
void BlockNode::ExtractJumps(JumpExprs *rets) const {
  stmts_.ExtractJumps(rets);
}

std::vector<ir::Val> BlockNode::EmitIR(Context *ctx) {
  stmts_.EmitIR(ctx);
  block_scope_->MakeAllDestructions(ctx);
  return {};
}
std::vector<ir::RegisterOr<ir::Addr>> BlockNode::EmitLVal(Context *) {
  UNREACHABLE();
}

}  // namespace ast
