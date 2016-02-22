#include "AST.h"

namespace AST {
  void Unop::join_identifiers(Scope* scope, bool is_arg) {
    if (expr_->is_identifier()) {
      expr_ = scope->identifier(expr_);
    } else {
      expr_->join_identifiers(scope);
    }
  }

  void While::join_identifiers(Scope* scope, bool is_arg) {
    cond_->join_identifiers(body_scope_);
    statements_->join_identifiers(body_scope_);
  }

  void ArrayLiteral::join_identifiers(Scope* scope, bool is_arg) {
    for (auto& el : elems_) {
      if (el->is_identifier()) {
        el = scope->identifier(el);
      } else {
        el->join_identifiers(scope);
      }
    }
  }

  void Terminal::join_identifiers(Scope* scope, bool is_arg) {}

  void Identifier::join_identifiers(Scope* scope, bool is_arg) {
    Terminal::join_identifiers(scope);
  }

  void Conditional::join_identifiers(Scope* scope, bool is_arg) {
    for (size_t i = 0; i < conds_.size(); ++i) {
      conds_[i]->join_identifiers(body_scopes_[i]);
    }

    for (size_t i = 0; i < statements_.size(); ++i) {
      statements_[i]->join_identifiers(body_scopes_[i]);
    }
  }

  void Access::join_identifiers(Scope* scope, bool is_arg) {
    if (expr_->is_identifier()) {
      expr_ = scope->identifier(expr_);
    } else {
      expr_->join_identifiers(scope);
    }
  }

  void Binop::join_identifiers(Scope* scope, bool is_arg) {
    if (lhs_->is_identifier()) {
      lhs_ = scope->identifier(lhs_);
    } else {
      lhs_->join_identifiers(scope);
    }

    // Ignore the RHS of a dot operator
    // TODO Access should be looking in a different scope
    // Should it be looking at all?
    // Should this even be a binary operator?
    if (op_ == Language::Operator::Access) return;

    if (rhs_->is_identifier()) {
      rhs_ = scope->identifier(rhs_);
    } else {
      rhs_->join_identifiers(scope);
    }
  }

  void ArrayType::join_identifiers(Scope* scope, bool is_arg) {
    if (len_ != nullptr) {
      if (len_->is_identifier()) {
        len_ = scope->identifier(len_);
      } else {
        len_->join_identifiers(scope);
      }
    }

    if (array_type_->is_identifier()) {
      array_type_ = scope->identifier(array_type_);
    } else {
      array_type_->join_identifiers(scope);
    }
  }

  void Declaration::join_identifiers(Scope* scope, bool is_arg) {
    id_ = std::static_pointer_cast<Identifier>(
        scope->identifier(declared_identifier()));
    if (is_arg) {
      id_->is_function_arg_ = true;
    }
    id_->line_num = line_num; // Hacky and probably wrong TODO FIXME

    if (decl_type_->is_identifier()) {
      decl_type_ = scope->identifier(decl_type_);
    } else {
      decl_type_->join_identifiers(scope);
    }
  }

  void ChainOp::join_identifiers(Scope* scope, bool is_arg) {
    for (auto& expr : exprs_) {
      if (expr->is_identifier()) {
        expr = scope->identifier(expr);
      } else {
        expr->join_identifiers(scope);
      }
    }
  }

  void Case::join_identifiers(Scope* scope, bool is_arg) {
    pairs_->join_identifiers(scope);
  }

  void KVPairList::join_identifiers(Scope* scope, bool is_arg) {
    for (auto& pair : kv_pairs_) {
      if (pair.first->is_identifier()) {
        pair.first = scope->identifier(pair.first);
      } else {
        pair.first->join_identifiers(scope);
      }

      if (pair.second->is_identifier()) {
        pair.second = scope->identifier(pair.second);
      } else {
        pair.second->join_identifiers(scope);
      }
    }
  }

  void Statements::join_identifiers(Scope* scope, bool is_arg) {
    for (auto& ptr : statements_) {
      if (ptr->is_identifier()) {
        ptr = std::static_pointer_cast<Node>(
            scope->identifier(
              std::static_pointer_cast<Expression>(ptr)
              )
            );
      } else {
        ptr->join_identifiers(scope);
      }
    }
  }

  void FunctionLiteral::join_identifiers(Scope* scope, bool is_arg) {
    for (auto& in : inputs_) {
      in->join_identifiers(fn_scope_, true);
    }

    if (return_type_->is_identifier()) {
      return_type_ = scope->identifier(return_type_);
    } else {
      return_type_->join_identifiers(fn_scope_);
    }

    statements_->join_identifiers(fn_scope_);
  }

  void TypeLiteral::join_identifiers(Scope*, bool) {
    for (auto& decl : decls_) {
      decl->join_identifiers(type_scope_);
    }
  }

  void EnumLiteral::join_identifiers(Scope* scope, bool is_arg) {}
}  // namespace AST
