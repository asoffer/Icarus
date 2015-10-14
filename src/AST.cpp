#include "AST.h"

namespace AST {


  /****************************************
   *            MISCELLANEOUS             *
   ****************************************/


  /* ANONYMOUS SCOPE */
//  void AnonymousScope::add_statements(NPtr&& stmts_ptr) {
//    auto stmts = std::unique_ptr<Statements>(
//        static_cast<Statements*>(stmts_ptr.release()));
//
//    statements_->statements_.reserve( statements_->size() + stmts->size() );
//
//    for (size_t i = 0; i < stmts->size(); ++i) {
//      statements_->statements_.push_back(std::move(stmts->statements_[i]));
//    }
//  }
//
//  void AnonymousScope::collect_return_types(std::set<Type>* return_exprs) const {
//    for (const auto& stmt : statements_->statements_) {
//      // TODO When we start having loops/conditionals, this won't cut it. We'll
//      // need to dive deeper into the scopes
//      if (!stmt->is_return()) continue;
//
//      // Safe because, to pass is_return(), it must be a pointer to a Unop.
//      auto unop_ptr = static_cast<Unop*>(stmt.get());
//      return_exprs->insert(unop_ptr->expr_->type());
//    }
//  }

  /* SCOPE */




}  // namespace AST
