#include "AST.h"

namespace AST {
  void Unop::assign_decl_to_scope(Scope* scope) {
    expr_->assign_decl_to_scope(scope);
  }

  void While::assign_decl_to_scope(Scope* scope) {
    // TODO implement
  }

  void Terminal::assign_decl_to_scope(Scope* scope) {}

  void Binop::assign_decl_to_scope(Scope* scope) {
    lhs_->assign_decl_to_scope(scope);
    rhs_->assign_decl_to_scope(scope);
  }

  void Declaration::assign_decl_to_scope(Scope* scope) {
    scope_ = scope;
    id_->assign_decl_to_scope(scope);
    decl_type_->assign_decl_to_scope(scope);
  }

  void ChainOp::assign_decl_to_scope(Scope* scope) {
    for (auto& expr : exprs_) {
      expr->assign_decl_to_scope(scope);
    }
  }

  void Case::assign_decl_to_scope(Scope* scope) {
    pairs_->assign_decl_to_scope(scope);
  }

  void KVPairList::assign_decl_to_scope(Scope* scope) {
    for (auto& pair : kv_pairs_) {
      pair.first->assign_decl_to_scope(scope);
      pair.second->assign_decl_to_scope(scope);
    }
  }

  void Statements::assign_decl_to_scope(Scope* scope) {
    for (auto& eptr : statements_) {
      eptr->assign_decl_to_scope(scope);
    }
  }

  void FunctionLiteral::assign_decl_to_scope(Scope* scope) {
    fn_scope_->set_parent(scope);

    for (auto& in : inputs_) {
      in->assign_decl_to_scope(fn_scope_);
    }
    statements_->assign_decl_to_scope(fn_scope_);
  }

}  // namespace AST
