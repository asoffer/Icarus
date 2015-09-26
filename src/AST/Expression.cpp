#include "AST/Expression.h"

namespace AST {
  std::map<std::string, size_t> prec_map = {
    { "*", 1 }
  };

  std::string Expression::to_string(size_t n) const {
    std::string spaces;
    for (size_t i = 0; i < n; ++i) {
      spaces += "  ";
    }
 
    return spaces + "[Expression: " + token_ + "]";
  }

  std::string Binop::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }
    output += "[Operator: " + token_ + "]\n";

    output += lhs_->to_string(n + 1) + "\n";
    output += rhs_->to_string(n + 1);

    return output;
  }
}  // namespace AST
