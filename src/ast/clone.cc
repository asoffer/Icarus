#include "ast.h"
#include "../base/util.h"

namespace AST {
Binop *Binop::Clone() const {
  auto *result = new Binop;
  result->span = span;
  result->lhs  = base::wrap_unique(lhs->Clone());
  result->rhs  = base::wrap_unique(rhs->Clone());
  result->op   = op;
  return result;
}

Access *Access::Clone() const {
  auto *result        = new Access;
  result->span        = span;
  result->operand     = base::wrap_unique(operand->Clone());
  result->member_name = member_name;
  return result;
}

ChainOp *ChainOp::Clone() const {
  auto *result = new ChainOp;
  result->span     = span;
  result->ops  = ops;
  result->exprs.reserve(exprs.size());
  for (const auto &expr : exprs) { result->exprs.emplace_back(expr->Clone()); }
  return result;
}

CommaList *CommaList::Clone() const {
  auto *result = new CommaList;
  result->span     = span;
  result->exprs.reserve(exprs.size());
  for (const auto &expr : exprs) { result->exprs.emplace_back(expr->Clone()); }
  return result;
}

} // namespace AST
