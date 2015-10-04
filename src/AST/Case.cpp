#include "AST/Case.h"

namespace AST {
  std::string Case::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }

    return output + "<Case>\n" + pairs_->to_string(n + 1);
  }

  void Case::join_identifiers(Scope* scope) {
    pairs_->join_identifiers(scope);
  }

  void Case::verify_types() {
    pairs_->verify_types_with_key(Type::Bool);
  }


}  // namespace AST
