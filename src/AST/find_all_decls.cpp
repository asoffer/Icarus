#include "AST.h"

namespace AST {
  void Unop::find_all_decls(Scope* scope) {
    expr_->find_all_decls(scope);
  }

  void Binop::find_all_decls(Scope* scope) {
    lhs_->find_all_decls(scope);
    rhs_->find_all_decls(scope);
  }

  void ChainOp::find_all_decls(Scope* scope) {
    for (auto& expr : exprs_) {
      expr->find_all_decls(scope);
    }
  }

//  void AnonymousScope::find_all_decls(Scope* scope) {
//    statements_->find_all_decls(scope);
//  } 

  void FunctionLiteral::find_all_decls(Scope*) {
    statements_->find_all_decls(&fn_scope_);
  } 

  void Declaration::find_all_decls(Scope* scope) {
    scope->register_local(this);
  }

  void Statements::find_all_decls(Scope* scope) {
    for (auto& eptr : statements_) {
      eptr->find_all_decls(scope);
    }
  }

  void Case::find_all_decls(Scope*) { // TODO
  }

}  // namespace AST
