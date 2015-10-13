#include "AST.h"

namespace AST {
  void Scope::register_scopes(Scope* parent_scope) {
    // TODO is this correct?
    if (this != parent_scope) {
      parent_ = parent_scope;
    }
    scope_registry.push_back(this);
  }

  void Unop::register_scopes(Scope* parent_scope) {
    expr_->register_scopes(parent_scope);
  }

  void Binop::register_scopes(Scope* parent_scope) {
    lhs_->register_scopes(parent_scope);
    rhs_->register_scopes(parent_scope);
  }

  void ChainOp::register_scopes(Scope* parent_scope) {
    for (auto& expr : exprs_) {
      expr->register_scopes(parent_scope);
    }
  }

  void Case::register_scopes(Scope* parent_scope) {
    pairs_->register_scopes(parent_scope);
  }

  void KVPairList::register_scopes(Scope* parent_scope) {
    for (const auto& kv : kv_pairs_) {
      kv.first->register_scopes(parent_scope);
      kv.second->register_scopes(parent_scope);
    }
  }

  void AnonymousScope::register_scopes(Scope* parent_scope) {
    Scope::register_scopes(parent_scope);
    statements_->register_scopes(this);
  }

  
  void Statements::register_scopes(Scope* parent_scope) {
    for (const auto& stmt : statements_) {
      stmt->register_scopes(parent_scope);
    }
  }

  void FunctionLiteral::register_scopes(Scope* parent_scope) {
    // This calls Scope::register_scope
    AnonymousScope::register_scopes(parent_scope);

    // FIXME This is almost certainly unnecessary as scopes probably cannot be
    // contained inside a declaration.
    for (const auto& decl : inputs_) {
      decl->register_scopes(this);
    }

  }

}  // namespace AST
