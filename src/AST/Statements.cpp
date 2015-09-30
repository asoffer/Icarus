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

  void Statements::join_identifiers(Scope* scope) {
    for (auto& eptr : statements_) {
      eptr->join_identifiers(scope);
    }
  }


}  // namespace AST
