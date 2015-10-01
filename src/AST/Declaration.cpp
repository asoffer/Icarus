#include "Declaration.h"

namespace AST {
  std::string Declaration::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }
    output += "<Declaration>\n";
    output += lhs_->to_string(n + 1);
    output += rhs_->to_string(n + 1);

    return output;
  }
}  // namespace AST
