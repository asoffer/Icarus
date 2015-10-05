#include "AST/Binop.h"
#include "AST/Identifier.h"

namespace AST {
  std::string Binop::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }
    output += "<Binop (" + expr_type_.to_string() + "): '" + (token_ == "" ? Language::show_name.at(type_) : token_) + "', prec: " + std::to_string(precedence_) + ">\n";

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

    if (lhs_->expr_type_ == Type::TypeError || rhs_->expr_type_ == Type::TypeError) {
      // An error was already found in the types, so just pass silently
      expr_type_ = Type::TypeError;
      return;
    }

    if (token_ == "=>") {
      if (lhs_->expr_type_ != Type::Bool)
        expr_type_ = Type::TypeError;

    } else if (token_ == ":>") {
      // TODO verify that this cast is possible
      expr_type_ = Type::Literals.at(rhs_->token());

    } else if (token_ == "<" || token_ == ">" || token_ == "<=" ||
        token_ == ">=" || token_ == "==" || token_ == "!=") {
      if (lhs_->expr_type_ != rhs_->expr_type_) {
        // If the types don't match give an error message. We can continue
        // because the result must be a bool
        std::cerr
          << "Type mismatch for comparison operator" << token_ << " ("
          << lhs_->expr_type_.to_string() << " and "
          << rhs_->expr_type_.to_string() << ")" << std::endl;
      }

      expr_type_ = Type::Bool;

    } else if (lhs_->expr_type_ == rhs_->expr_type_) {
      //Otherwise it's an arithmetic operator
      expr_type_ = lhs_->expr_type_;

    }
    else {
      // TODO give a type-mismatch error here
      expr_type_ = Type::TypeError;
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
      auto id_ptr = scope->identifier(rhs_->token());
      rhs_ = std::static_pointer_cast<Expression>(id_ptr);
    } else {
      rhs_->join_identifiers(scope);
    }
  }

}  // namespace AST
