/*
#include "AST.h"

namespace AST {
  void Terminal::register_identifiers(DeclPtr decl_ptr) {
  }

  void Unop::register_identifiers(DeclPtr decl_ptr) {
    if (expr_->is_identifier()) {
      decl_ptr->needs_identifier(expr_->token());
    }
    expr_->register_identifiers(decl_ptr);
  }

  void Binop::register_identifiers(DeclPtr decl_ptr) {
    if (lhs_->is_identifier()) {
      decl_ptr->needs_identifier(lhs_->token());
    }

    if (rhs_->is_identifier()) {
      decl_ptr->needs_identifier(rhs_->token());
    }
 
    lhs_->register_identifiers(scope);
    rhs_->register_identifiers(scope);
  }

  void ChainOp::register_identifiers(DeclPtr decl_ptr) {
    for (auto& expr : exprs_) {
      expr->register_identifiers(scope);
    }
  }

  void FunctionLiteral::register_identifiers(Scope*) {
    statements_->register_identifiers(&fn_scope_);
  } 

  void Declaration::register_identifiers(DeclPtr decl_ptr) {
    scope->register_local(this);
  }

  void Case::register_identifiers(DeclPtr) { // TODO
  }

}  // namespace AST
*/
