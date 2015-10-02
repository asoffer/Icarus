#include "Declaration.h"

namespace AST {
  void Declaration::verify_types() {
    expr_type_ = lhs_->expr_type_;
  }

  void Declaration::find_all_decls(Scope* scope) {
    scope->register_declaration(this);
  }

  std::string Declaration::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }
    output += "<Declaration (" + std::to_string(expr_type_) + ")>\n";
    output += lhs_->to_string(n + 1);
    output += rhs_->to_string(n + 1);

    return output;
  }
}  // namespace AST
