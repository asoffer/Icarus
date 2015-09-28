#include "AST/Binop.h"

namespace AST {
  std::string Binop::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }
    output += "<Binop: '" + (token_ == "" ? Node::debug_map[type_] : token_) + "', prec: " + std::to_string(precedence_) + ">\n";
    output += lhs_->to_string(n + 1);
    output += rhs_->to_string(n + 1);

    return output;
  }
}  // namespace AST
