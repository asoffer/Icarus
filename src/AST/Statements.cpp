#include "AST/Statements.h"

namespace AST {
  std::string Statements::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }

    output += "<Statements>\n";
    for (const auto& exprs : statements_) {
      output += exprs->to_string(n + 1);
    }
    return output;
  }

  void Statements::find_all_decls(Scope* scope) {
    for (auto& eptr : statements_) {
      eptr->find_all_decls(scope);
    }
  }
 
  void Statements::verify_types() {
    for (auto& eptr : statements_) {
      eptr->verify_types();
    }
  }
 
  void Statements::join_identifiers(Scope* scope) {
    for (auto& eptr : statements_) {
      eptr->join_identifiers(scope);
    }
  }


}  // namespace AST
