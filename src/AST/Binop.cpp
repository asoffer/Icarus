#include "AST/Binop.h"

namespace AST {
  std::string Binop::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }
    output += "<Binop: '" + (token_ == "" ? Node::debug_map[type_] : token_) + "', prec: " + std::to_string(precedence_) + ">\t\t{ ";

    for (const auto& id : identifiers()) {
      output += id + " ";
    }

    output += "}\n";

    output += lhs_->to_string(n + 1);
    output += rhs_->to_string(n + 1);

    return output;
  }

  void Binop::separate_declarations_and_assignments() {
    if (token_ == "=" && lhs_->is_binop() && static_cast<Binop*>(lhs_.get())->token_ == ":") {
      std::cout << "######" << std::endl;
      std::cout << to_string(0) << std::endl;
      std::cout << "######" << std::endl;
    }
  }

  void Binop::verify_no_declarations() const {
    if (token_ != ":") {
      lhs_->verify_no_declarations();
      rhs_->verify_no_declarations();
    }
    else {
      throw "Found a declaration in a bad place";
    }
  }
}  // namespace AST
