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

void BlockNode::assign_scope(core::Scope *scope) {
  scope_ = scope;
  name_->assign_scope(scope);
  block_scope_ = scope->add_child<core::ExecScope>();
  stmts_.assign_scope(block_scope_.get());
}

void BlockNode::DependentDecls(DeclDepGraph *g,
                               Declaration *d) const {
  name_->DependentDecls(g, d);
  stmts_.DependentDecls(g, d);
}

VerifyResult BlockNode::VerifyType(Context *ctx) {
  stmts_.VerifyType(ctx);
  return ctx->set_result(this, VerifyResult::Constant(type::Blk()));
}
void BlockNode::ExtractJumps(JumpExprs *rets) const {
  stmts_.ExtractJumps(rets);
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
