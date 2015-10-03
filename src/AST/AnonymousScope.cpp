#include "AST/AnonymousScope.h"

namespace AST {
  std::string AnonymousScope::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }

    output += "<Anonymous Scope>\n";

    return output + statements_->to_string(n + 1);
  }

  void AnonymousScope::add_statements(NPtr&& stmts_ptr) {
    auto stmts = std::unique_ptr<Statements>(static_cast<Statements*>(stmts_ptr.release()));
    statements_->statements_.reserve( statements_->size() + stmts->size() );

    for (size_t i = 0; i < stmts->size(); ++i) {
      statements_->statements_.push_back(std::move(stmts->statements_[i]));
    }
  }

  void AnonymousScope::join_identifiers(Scope* scope) {
    statements_->join_identifiers(scope);
  }

  void AnonymousScope::verify_types() {
    statements_->verify_types();
  }
   void AnonymousScope::find_all_decls(Scope* scope) {
    statements_->find_all_decls(scope);
   } 
}  // namespace AST
