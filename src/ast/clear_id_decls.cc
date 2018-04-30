#include "ast.h"

namespace AST {
void Access::ClearIdDecls() {
  stage_range_ = StageRange{};
  operand->ClearIdDecls();
}
void Identifier::ClearIdDecls() {
  stage_range_ = StageRange{};
  decl         = nullptr;
}
void Terminal::ClearIdDecls() { stage_range_ = StageRange{}; }

void Binop::ClearIdDecls() {
  stage_range_ = StageRange{};
  lhs->ClearIdDecls();
  rhs->ClearIdDecls();
}

void Declaration::ClearIdDecls() {
  stage_range_ = StageRange{};
  identifier->ClearIdDecls();
  if (type_expr) { type_expr->ClearIdDecls(); }
  if (init_val) { init_val->ClearIdDecls(); }
}

void ChainOp::ClearIdDecls() {
  stage_range_ = StageRange{};
  for (auto &expr : exprs) { expr->ClearIdDecls(); }
}

void CommaList::ClearIdDecls() {
  stage_range_ = StageRange{};
  for (auto &expr : exprs) { expr->ClearIdDecls(); }
}

void Statements::ClearIdDecls() {
  stage_range_ = StageRange{};
  for (auto &stmt : content_) { stmt->ClearIdDecls(); }
}

void GenericFunctionLiteral::ClearIdDecls() {
  stage_range_ = StageRange{};
  FunctionLiteral::ClearIdDecls();
}

void FunctionLiteral::ClearIdDecls() {
  fn_scope = nullptr;
  stage_range_ = StageRange{};
  for (auto &in : inputs) { in->ClearIdDecls(); }
  for (auto &out : outputs) { out->ClearIdDecls(); }
  statements->ClearIdDecls();
}

void Call::ClearIdDecls() {
  stage_range_ = StageRange{};
  fn_->ClearIdDecls();
  args_.Apply([](auto &expr) { expr->ClearIdDecls(); });
}

void ScopeNode::ClearIdDecls() {
  stage_range_ = StageRange{};
  scope_expr->ClearIdDecls();
  if (expr) { expr->ClearIdDecls(); }
  stmts->ClearIdDecls();
}

void ScopeLiteral::ClearIdDecls() {
  stage_range_ = StageRange{};
  if (enter_fn) { enter_fn->ClearIdDecls(); }
  if (exit_fn) { exit_fn->ClearIdDecls(); }
}

void StructLiteral::ClearIdDecls() {
  stage_range_ = StageRange{};
  for (auto& f : fields_) { f->ClearIdDecls(); }
}

} // namespace AST
