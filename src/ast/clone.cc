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

Declaration * Declaration::Clone() const {
  auto *result       = new Declaration;
  result->span       = span;
  result->const_     = const_;
  result->identifier = base::wrap_unique(identifier->Clone());
  result->type_expr =
      type_expr ? base::wrap_unique(type_expr->Clone()) : nullptr;
  result->init_val = init_val ? base::wrap_unique(init_val->Clone()) : nullptr;
  return result;
}

Access *Access::Clone() const {
  auto *result        = new Access;
  result->span        = span;
  result->operand     = base::wrap_unique(operand->Clone());
  result->member_name = member_name;
  return result;
}

FunctionLiteral *FunctionLiteral::Clone() const {
  auto *result       = new FunctionLiteral;
  result->span       = span;
  result->statements = base::wrap_unique(statements->Clone());
  result->lookup_    = lookup_;
  result->inputs.reserve(inputs.size());
  for (const auto &input : inputs) {
    result->inputs.emplace_back(input->Clone());
  }
  for (const auto &output : outputs) {
    result->outputs.emplace_back(output->Clone());
  }
  return result;
}

GenericFunctionLiteral *GenericFunctionLiteral::Clone() const { UNREACHABLE(); }

ScopeLiteral *ScopeLiteral::Clone() const {
  auto *result     = new ScopeLiteral;
  result->span     = span;
  result->enter_fn = base::wrap_unique(enter_fn->Clone());
  result->exit_fn  = base::wrap_unique(exit_fn->Clone());
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

StructLiteral *StructLiteral::Clone() const {
  auto *result = new StructLiteral;
  result->span = span;
  result->fields_.reserve(fields_.size());
  for (const auto &f : fields_) { result->fields_.emplace_back(f->Clone()); }
  return result;
}

} // namespace AST
