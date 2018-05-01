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

void Declaration::ExtractReturns(std::vector<const Expression *> *rets) const {
  identifier->ExtractReturns(rets);
  if (type_expr) { type_expr->ExtractReturns(rets); }
  if (init_val) { init_val->ExtractReturns(rets); }
}

void ChainOp::ExtractReturns(std::vector<const Expression *> *rets) const {
  for (auto &expr : exprs) { expr->ExtractReturns(rets); }
}

void CommaList::ExtractReturns(std::vector<const Expression *> *rets) const {
  for (auto &expr : exprs) { expr->ExtractReturns(rets); }
}

void GenericFunctionLiteral::ExtractReturns(
    std::vector<const Expression *> *rets) const {
  FunctionLiteral::ExtractReturns(rets);
}

void FunctionLiteral::ExtractReturns(
    std::vector<const Expression *> *rets) const {
  for (auto &in : inputs) { in->ExtractReturns(rets); }
  for (auto &out : outputs) { out->ExtractReturns(rets); }
}

void ScopeLiteral::ExtractReturns(std::vector<const Expression *> *rets) const {
  if (enter_fn) { enter_fn->ExtractReturns(rets); }
  if (exit_fn) { exit_fn->ExtractReturns(rets); }
}

void StructLiteral::ExtractReturns(
    std::vector<const Expression *> *rets) const {
  for (auto &f : fields_) { f->ExtractReturns(rets); }
}
}  // namespace AST
