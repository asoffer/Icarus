#include "AST.h"

namespace AST {
  void Unop::join_identifiers(Scope* scope) {
    if (expr_->is_identifier()) {
      auto id_ptr = scope->get_identifier(expr_->token());
      expr_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      expr_->join_identifiers(scope);
    }
  }

  void While::join_identifiers(Scope* scope) {
    if (cond_->is_identifier()) {
      auto id_ptr = scope->get_identifier(cond_->token());
      cond_ = std::static_pointer_cast<Expression>(id_ptr);
    } else {
      cond_->join_identifiers(scope);
    }

    statements_->join_identifiers(&body_scope_);
  }

  void Binop::join_identifiers(Scope* scope) {
    if (lhs_->is_identifier()) {
      auto id_ptr = scope->get_identifier(lhs_->token());
      lhs_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      lhs_->join_identifiers(scope);
    }

    if (rhs_->is_identifier()) {
      auto id_ptr = scope->get_identifier(rhs_->token());
      rhs_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      rhs_->join_identifiers(scope);
    }
  }


  void ChainOp::join_identifiers(Scope* scope) {
    for (auto& expr : exprs_) {
      if (expr->is_identifier()) {
        auto id_ptr = scope->get_identifier(expr->token());
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
    for (const auto& pair : kv_pairs_) {
      pair.first->join_identifiers(scope);
      pair.second->join_identifiers(scope);
    }
  }

  void Statements::join_identifiers(Scope* scope) {
    for (auto& eptr : statements_) {
      if (eptr->is_identifier()) {
        auto id_ptr = scope->get_identifier(eptr->token());
        eptr = std::static_pointer_cast<Node>(id_ptr);
      }

      eptr->join_identifiers(scope);
    }
  }

  void FunctionLiteral::join_identifiers(Scope*) {
    statements_->join_identifiers(&fn_scope_);
  }

}  // namespace AST
