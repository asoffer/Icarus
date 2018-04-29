#include "ast/ast.h"

#include <set>
namespace AST {
void Unop::ExtractReturns(std::vector<const Expression *> *rets) const {
  operand->ExtractReturns(rets);
  if (op == Language::Operator::Return) { rets->push_back(operand.get()); }
}

void Access::ExtractReturns(std::vector<const Expression *> *rets) const {
  operand->ExtractReturns(rets);
}

void Identifier::ExtractReturns(std::vector<const Expression *> *) const {}
void Terminal::ExtractReturns(std::vector<const Expression *> *) const {}

void ArrayType::ExtractReturns(std::vector<const Expression *> *rets) const {
  // TODO length needs to be constexpr so maybe we're safe here? and don't need
  // to check it? This happens in other places too!
  length->ExtractReturns(rets);
  data_type->ExtractReturns(rets);
}

void ArrayLiteral::ExtractReturns(std::vector<const Expression *> *rets) const {
  for (auto &el : elems) { el->ExtractReturns(rets); }
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

void Statements::ExtractReturns(std::vector<const Expression *> *rets) const {
  for (auto &stmt : content_) { stmt->ExtractReturns(rets); }
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

void Call::ExtractReturns(std::vector<const Expression *> *rets) const {
  fn_->ExtractReturns(rets);
  for (const auto &val : args_.pos_) { val->ExtractReturns(rets); }
  for (const auto & [ key, val ] : args_.named_) { val->ExtractReturns(rets); }
}

void ScopeNode::ExtractReturns(std::vector<const Expression *> *rets) const {
  scope_expr->ExtractReturns(rets);
  if (expr) { expr->ExtractReturns(rets); }
  stmts->ExtractReturns(rets);
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
