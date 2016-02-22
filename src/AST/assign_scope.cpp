#include "AST.h"

namespace AST {
  void Unop::assign_scope(Scope* scope) {
    scope_ = scope;
    operand->assign_scope(scope);
  }

  void Conditional::assign_scope(Scope* scope) {
    scope_ = scope;
    for (size_t i = 0; i < conds_.size(); ++i) {
      body_scopes_[i]->set_parent(scope);
      conds_[i]->assign_scope(scope);
      statements[i]->assign_scope(body_scopes_[i]);
    }

    if (has_else()) {
      body_scopes_.back()->set_parent(scope);
      statements.back()->assign_scope(body_scopes_.back());
    }
  }

  void While::assign_scope(Scope* scope) {
    scope_ = scope;
    while_scope->set_parent(scope);
    condition->assign_scope(while_scope);
    statements->assign_scope(while_scope);
  }

  void ArrayLiteral::assign_scope(Scope* scope) {
    scope_ = scope;
    for (auto &el : elems) {
      el->assign_scope(scope);
    }
  }

  void Terminal::assign_scope(Scope* scope) { scope_ = scope; }

  void Identifier::assign_scope(Scope* scope) {
    // We want scope_ to be the scope the identifier was declared in. If we
    // follow the DAG to do the assignment, we may write to it many times, and
    // if the last time we write is not for its declaration, we lose.
  }

  void Access::assign_scope(Scope* scope) {
    scope_ = scope;
    operand->assign_scope(scope);
  }

  void Binop::assign_scope(Scope* scope) {
    scope_ = scope;
    lhs->assign_scope(scope);
    rhs->assign_scope(scope);
  }

  void Declaration::assign_scope(Scope* scope) {
    scope_ = scope;

    scope_->ids_[identifier->token()] = identifier;
    identifier->decl = this;

    identifier->scope_ = scope;
    type_expr->assign_scope(scope);
  }

  void ArrayType::assign_scope(Scope* scope) {
    scope_ = scope;
  }

  void ChainOp::assign_scope(Scope* scope) {
    scope_ = scope;
    for (auto& expr : exprs) {
      expr->assign_scope(scope);
    }
  }

  void Case::assign_scope(Scope* scope) {
    scope_ = scope;
    kv->assign_scope(scope);
  }

  void KVPairList::assign_scope(Scope* scope) {
    scope_ = scope;
    for (auto& pair : pairs) {
      pair.first->assign_scope(scope);
      pair.second->assign_scope(scope);
    }
  }

  void Statements::assign_scope(Scope* scope) {
    scope_ = scope;
    for (auto& nptr : statements) {
      nptr->assign_scope(scope);
    }
  }

  void FunctionLiteral::assign_scope(Scope* scope) {
    scope_ = scope;
    fn_scope_->set_parent(scope);

    for (auto& in : inputs_) {
      in->assign_scope(fn_scope_);
    }
    statements->assign_scope(fn_scope_);
  }

  void TypeLiteral::assign_scope(Scope* scope) {
    scope_ = scope;
    type_scope->set_parent(scope);
    for (auto& decl : declarations) {
      decl->assign_scope(type_scope);
    }
  }

  void EnumLiteral::assign_scope(Scope* scope) {
    scope_ = scope;
  }
}  // namespace AST
