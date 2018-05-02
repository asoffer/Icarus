#include "ast.h"

namespace AST {
void Access::ClearIdDecls() {
  stage_range_ = StageRange{};
  operand->ClearIdDecls();
}

void Binop::ClearIdDecls() {
  stage_range_ = StageRange{};
  lhs->ClearIdDecls();
  rhs->ClearIdDecls();
}

void ChainOp::ClearIdDecls() {
  stage_range_ = StageRange{};
  for (auto &expr : exprs) { expr->ClearIdDecls(); }
}

void CommaList::ClearIdDecls() {
  stage_range_ = StageRange{};
  for (auto &expr : exprs) { expr->ClearIdDecls(); }
}

} // namespace AST
