#include "AST/Expression.h"

namespace AST {
  std::map<std::string, size_t> prec_map = {
    { ":",  1 },
    { "=>", 2 },
    { "->", 2 },
    { "<",  3 },
    { ">",  3 },
    { "<=", 3 },
    { ">=", 3 },
    { "==", 3 },
    { "!=", 3 },
    { "+",  4 },
    { "-",  4 },
    { "*",  5 },
    { "/",  5 },
    { "%",  5 }
  };

  NPtr Binop::fix_tree_precedence(bool return_ptr) {
    if (precedence_ <= lhs_->precedence_) {
      return NPtr(return_ptr ? this : nullptr);
    }

    // TODO (for equality check associativity)

    auto lhs_ptr = static_cast<Binop*>(lhs_.release());

    lhs_ = std::unique_ptr<Expression>(lhs_ptr->rhs_.release());

    lhs_ptr->rhs_ = std::unique_ptr<Expression>(this);

    // Recurse
    // TODO(andy) it would be faster to find the correct place to insert and
    // then just do that. Fix this on the off chance that this is a bottleneck
    fix_tree_precedence(false);

    return NPtr(lhs_ptr);
  }

  std::string Expression::to_string(size_t) const {
    return "==========";
  }

  std::string Terminal::to_string(size_t n) const {
    std::string spaces;
    for (size_t i = 0; i < n; ++i) {
      spaces += "  ";
    }
 
    return spaces + "<" + Node::debug_map[base_type_] + ": " + token_ + ">";
  }

  std::string Binop::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }
    output += "<Binop: '" + token_ + "'>\n";

    output += lhs_->to_string(n + 1) + "\n";
    output += rhs_->to_string(n + 1);

    return output;
  }
}  // namespace AST
