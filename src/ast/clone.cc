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

Call *Call::Clone() const {
  auto *result            = new Call;
  result->span = span;
  result->fn_             = base::wrap_unique(fn_->Clone());
  result->args_.pos_.reserve(args_.pos_.size());
  for (const auto &val : args_.pos_) {
    result->args_.pos_.emplace_back(val->Clone());
  }
  for (const auto & [ key, val ] : args_.named_) {
    result->args_.named_.emplace(key, base::wrap_unique(val->Clone()));
  }


  result->dispatch_table_ = dispatch_table_;
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

InDecl *InDecl::Clone() const {
  auto *result      = new InDecl;
  result->span      = span;
  result->container = base::wrap_unique(container->Clone());
  return result;
}

Unop *Unop::Clone() const {
  auto *result    = new Unop;
  result->span    = span;
  result->operand = base::wrap_unique(operand->Clone());
  result->op      = op;
  return result;
}

Access *Access::Clone() const {
  auto *result        = new Access;
  result->span        = span;
  result->operand     = base::wrap_unique(operand->Clone());
  result->member_name = member_name;
  return result;
}

ArrayType *ArrayType::Clone() const {
  auto *result      = new ArrayType;
  result->span      = span;
  result->length    = base::wrap_unique(length->Clone());
  result->data_type = base::wrap_unique(data_type->Clone());
  return result;
}

FunctionLiteral *FunctionLiteral::Clone() const {
  auto *result             = new FunctionLiteral;
  result->span      = span;
  result->return_type_expr = base::wrap_unique(return_type_expr->Clone());
  result->statements       = base::wrap_unique(statements->Clone());
  result->lookup_          = lookup_;
  result->inputs.reserve(inputs.size());
  for (const auto &input : inputs) {
    result->inputs.emplace_back(input->Clone());
  }
  return result;
}

GenericFunctionLiteral *GenericFunctionLiteral::Clone() const { UNREACHABLE(); }

For *For::Clone() const {
  auto *result = new For;
  result->span = span;
  result->iterators.reserve(iterators.size());
  for (const auto &input : iterators) {
    result->iterators.emplace_back(input->Clone());
  }
  result->statements = base::wrap_unique(statements->Clone());

  return result;
}

ScopeNode *ScopeNode::Clone() const {
  auto *result       = new ScopeNode;
  result->span = span;
  result->expr       = base::wrap_unique(expr->Clone());
  result->scope_expr = base::wrap_unique(scope_expr->Clone());
  result->stmts      = base::wrap_unique(stmts->Clone());
  return result;
}

ScopeLiteral *ScopeLiteral::Clone() const {
  auto *result     = new ScopeLiteral;
  result->span     = span;
  result->enter_fn = base::wrap_unique(enter_fn->Clone());
  result->exit_fn  = base::wrap_unique(exit_fn->Clone());
  return result;
}

Case *Case::Clone() const {
  auto *result = new Case;
  result->span     = span;
  result->key_vals.reserve(key_vals.size());
  for (const auto & [ key, val ] : key_vals) {
    result->key_vals.emplace_back(base::wrap_unique(key->Clone()),
                                  base::wrap_unique(val->Clone()));
  }
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

ArrayLiteral *ArrayLiteral::Clone() const {
  auto *result = new ArrayLiteral;
  result->span     = span;
  result->elems.reserve(elems.size());
  for (const auto &elem : elems) { result->elems.emplace_back(elem->Clone()); }
  return result;
}

Identifier *Identifier::Clone() const {
  auto *result      = new Identifier;
  result->span     = span;
  result->token     = token;
  Declaration *decl = nullptr;
  return result;
}

TokenNode *TokenNode::Clone() const { return new TokenNode(*this); }
Terminal *Terminal::Clone() const { return new Terminal(*this); }
Hole *Hole::Clone() const { return new Hole(*this); }
Statements *Statements::Clone() const { return new Statements(*this); }
CodeBlock *CodeBlock::Clone() const { return new CodeBlock(*this); }
Jump *Jump::Clone() const { return new Jump(*this); }
} // namespace AST
