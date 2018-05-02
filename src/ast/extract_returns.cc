#include "ast/ast.h"

#include <set>
namespace AST {
void Access::ExtractReturns(std::vector<const Expression *> *rets) const {
  operand->ExtractReturns(rets);
}

void Binop::ExtractReturns(std::vector<const Expression *> *rets) const {
  lhs->ExtractReturns(rets);
  rhs->ExtractReturns(rets);
}

void ChainOp::ExtractReturns(std::vector<const Expression *> *rets) const {
  for (auto &expr : exprs) { expr->ExtractReturns(rets); }
}

void CommaList::ExtractReturns(std::vector<const Expression *> *rets) const {
  for (auto &expr : exprs) { expr->ExtractReturns(rets); }
}

}  // namespace AST
