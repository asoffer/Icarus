#include "Assignment.h"

namespace AST {
  void Assignment::verify_types() {
    Binop::verify_types();

    if (lhs_->expr_type_ == type_error) return;
    if (rhs_->expr_type_ == type_error) return;

    if (lhs_->expr_type_ != rhs_->expr_type_) {
      std::cout << "!!!" << std::endl;
      // TODO Give some error about assignment type-mismatch
    }
    expr_type_ = t_void;
  }

  std::string Assignment::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }
    output += "<Assignment (" + std::to_string(expr_type_) + ")>\n";
    output += lhs_->to_string(n + 1);
    output += rhs_->to_string(n + 1);

    return output;
  }
}  // namespace AST
