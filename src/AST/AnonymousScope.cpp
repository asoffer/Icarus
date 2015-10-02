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
