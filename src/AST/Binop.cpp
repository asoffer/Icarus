#include "AST/Binop.h"
#include "AST/Identifier.h"

namespace AST {
  std::string Binop::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }
    output += "<Binop (" + std::to_string(expr_type_) + "): '" + (token_ == "" ? Language::show_name.at(type_) : token_) + "', prec: " + std::to_string(precedence_) + ">\n";

    output += lhs_->to_string(n + 1);
    output += rhs_->to_string(n + 1);

    return output;
  }

  void Binop::find_all_decls(Scope* scope) {
    lhs_->find_all_decls(scope);
    rhs_->find_all_decls(scope);
  }

  void Binop::verify_types() {
    // FIXME this is ugly, but "worse is better"
    // TODO make this better

    lhs_->verify_types();
    rhs_->verify_types();

    if (lhs_->expr_type_ == type_error || rhs_->expr_type_ == type_error) {
      // An error was already found in the types, so just pass silently
      expr_type_ = type_error;
      return;
    }

    if (token_ == "=>") {
      if (lhs_->expr_type_ != t_bool)
        expr_type_ = type_error;

    } else if (token_ == ":>") {
      expr_type_ = Language::type_literals.at(rhs_->token());

    } else if (token_ == "<" || token_ == ">" || token_ == "<=" ||
        token_ == ">=" || token_ == "==" || token_ == "!=") {
      if (lhs_->expr_type_ != rhs_->expr_type_) {
        expr_type_ = type_error;

      } else {
        expr_type_ = t_bool;
      }
    } else if (lhs_->expr_type_ == rhs_->expr_type_) {
      //Otherwise it's an arithmetic operator
      expr_type_ = lhs_->expr_type_;
    }
    else {
      // TODO give a type-mismatch error here
      expr_type_ = type_error;
    }
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
