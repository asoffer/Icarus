#include "AST.h"

namespace AST {
  void Unop::join_identifiers(size_t scope_id_num) {
    if (expr_->is_identifier()) {
      auto id_ptr = ScopeDB::identifier(scope_id_num, expr_->token());
      expr_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      expr_->join_identifiers(scope_id_num);
    }
  }

  void While::join_identifiers(size_t scope_id_num) {
    // TODO implement
  }

  void Binop::join_identifiers(size_t scope_id_num) {
    if (lhs_->is_identifier()) {
      auto id_ptr = ScopeDB::identifier(scope_id_num, lhs_->token());
      lhs_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      lhs_->join_identifiers(scope_id_num);
    }

    if (rhs_->is_identifier()) {
      auto id_ptr = ScopeDB::identifier(scope_id_num, rhs_->token());
      rhs_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      rhs_->join_identifiers(scope_id_num);
    }
  }

  void Declaration::join_identifiers(size_t scope_id_num) {
    id_ = ScopeDB::identifier(scope_id_num, identifier_string());

    if (decl_type_->is_identifier()) {
      auto id_ptr = ScopeDB::identifier(scope_id_num, decl_type_->token());
      decl_type_ = std::static_pointer_cast<Expression>(id_ptr);

    } else {
      decl_type_->join_identifiers(scope_id_num);
    }

  }

  void ChainOp::join_identifiers(size_t scope_id_num) {
    for (auto& expr : exprs_) {
      if (expr->is_identifier()) {
        auto id_ptr = ScopeDB::identifier(scope_id_num, expr->token());
        expr = std::static_pointer_cast<Expression>(id_ptr);

      } else {
        expr->join_identifiers(scope_id_num);
      }
    }
  }

  void Case::join_identifiers(size_t scope_id_num) {
    pairs_->join_identifiers(scope_id_num);
  }

  void KVPairList::join_identifiers(size_t scope_id_num) {
    for (const auto& pair : kv_pairs_) {
      pair.first->join_identifiers(scope_id_num);
      pair.second->join_identifiers(scope_id_num);
    }
  }

  void Statements::join_identifiers(size_t scope_id_num) {
    for (auto& eptr : statements_) {
      if (eptr->is_identifier()) {
        auto id_ptr = ScopeDB::identifier(scope_id_num, eptr->token());
        eptr = std::static_pointer_cast<Node>(id_ptr);
      }

      eptr->join_identifiers(scope_id_num);
    }
  }

  void FunctionLiteral::join_identifiers(size_t) {
    statements_->join_identifiers(scope_id_);
  }

}  // namespace AST
