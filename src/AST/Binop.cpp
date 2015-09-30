#include "AST/Binop.h"
#include "AST/Identifier.h"

namespace AST {
  std::string Binop::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }
    output += "<Binop: '" + (token_ == "" ? Language::show_name.at(type_) : token_) + "', prec: " + std::to_string(precedence_) + ">\n";

    output += lhs_->to_string(n + 1);
    output += rhs_->to_string(n + 1);

    return output;
  }

  void Binop::join_identifiers(Scope* scope) {
    if (lhs_->is_identifier()) {
      auto id_ptr = scope->identifier(lhs_->token());
      lhs_ = std::static_pointer_cast<Expression>(id_ptr);
    } else {
      lhs_->join_identifiers(scope);
    }

    if (rhs_->is_identifier()) {
      rhs_ = std::static_pointer_cast<Expression>(scope->identifier(rhs_->token()));
    } else {
      rhs_->join_identifiers(scope);
    }
  }

}  // namespace AST
