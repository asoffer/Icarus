#include "AST.h"

namespace AST {
  void Unop::join_identifiers(Scope* scope) {
    if (expr_->is_identifier()) {
      auto id_ptr = scope->identifier(expr_->token());
      expr_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      expr_->join_identifiers(scope);
    }
  }

  void While::join_identifiers(Scope* scope) {
    // TODO implement
  }

  void Binop::join_identifiers(Scope* scope) {
    if (lhs_->is_identifier()) {
      auto id_ptr = scope->identifier(lhs_->token());
      lhs_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      lhs_->join_identifiers(scope);
    }

    if (rhs_->is_identifier()) {
      auto id_ptr = scope->identifier(rhs_->token());
      rhs_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      rhs_->join_identifiers(scope);
    }
  }

  void Declaration::join_identifiers(Scope* scope) {
    id_ = scope->identifier(identifier_string());

    if (decl_type_->is_identifier()) {
      auto id_ptr = scope->identifier(decl_type_->token());
      decl_type_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      decl_type_->join_identifiers(scope);
    }

  }

  void ChainOp::join_identifiers(Scope* scope) {
    for (auto& expr : exprs_) {
      if (expr->is_identifier()) {
        auto id_ptr = scope->identifier(expr->token());
        expr = std::static_pointer_cast<Expression>(id_ptr);

      } else {
        expr->join_identifiers(scope);
      }
    }
  }

  void Case::join_identifiers(Scope* scope) {
    pairs_->join_identifiers(scope);
  }

  void KVPairList::join_identifiers(Scope* scope) {
    for (auto& pair : kv_pairs_) {
      if (pair.first->is_identifier()) {
        auto id_ptr = scope->identifier(pair.first->token());
        pair.second = std::static_pointer_cast<Expression>(id_ptr);
      } else {
        pair.first->join_identifiers(scope);
      }

      if (pair.second->is_identifier()) {
        auto id_ptr = scope->identifier(pair.second->token());
        pair.second = std::static_pointer_cast<Expression>(id_ptr);
      } else {
        pair.second->join_identifiers(scope);
      }
    }
  }

  void Statements::join_identifiers(Scope* scope) {
    for (auto& eptr : statements_) {
      if (eptr->is_identifier()) {
        auto id_ptr = scope->identifier(eptr->token());
        eptr = std::static_pointer_cast<Node>(id_ptr);
      }

      eptr->join_identifiers(scope);
    }
  }

  void FunctionLiteral::join_identifiers(Scope* scope) {
    fn_scope_->set_parent(scope);

    for (auto& in : inputs_) {
      in->join_identifiers(fn_scope_);
    }
    statements_->join_identifiers(fn_scope_);
  }

}  // namespace AST
