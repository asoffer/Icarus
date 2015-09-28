#include "AST/Case.h"

namespace AST {
  std::string Case::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }

    output += "<Case>\t\t{ ";
    for (const auto& id : identifiers()) {
      output += id + " ";
    }

    output += "}\n";
    output += pairs_->to_string(n);

    return output;
  }

  std::set<std::string> Case::identifiers() const {
    return pairs_->identifiers();
  } 
}  // namespace AST
