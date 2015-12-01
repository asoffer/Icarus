#include "AST.h"
#include "Type.h"
#include <sstream> // DEBUG

namespace AST {
  std::string tabs(size_t n) {
    // Tabs are two spaces
    return std::string(n << 1, ' ');
  }

  std::string Node::to_string(size_t n) const {
    std::string output =
      tabs(n) + "[" + Language::show_name.at(type_);

    if (!token_.empty())
      output += ": " + token_;

    return output + "]\n";
  }

  std::string Conditional::to_string(size_t n) const {
    return tabs(n) + "<Conditional>\n"
      + cond_->to_string(n + 1)
      + statements_->to_string(n + 1);
  }


  std::string ArrayLiteral::to_string(size_t n) const {
    std::string output = tabs(n) + "<ArrayLiteral>\n";
    for (const auto& el : elems_) {
      output += el->to_string(n + 1);
    }

    return output;
  }

  std::string While::to_string(size_t n) const {
    return tabs(n) + "<While>\n"
      + cond_->to_string(n + 1)
      + statements_->to_string(n + 1);
  }

  std::string Unop::to_string(size_t n) const {
    return tabs(n) + "<Unop " + expr_type_->to_string() + ": '"
      + (token_ == "" ? Language::show_name.at(type_) : token_)
      + "', prec: " + std::to_string(precedence_) + ">\n"
      + expr_->to_string(n + 1);
  }

  std::string Binop::to_string(size_t n) const {
    std::string output = 
      tabs(n) + "<Binop " + expr_type_->to_string() + ": '"
      + (token_ == "" ? Language::show_name.at(type_) : token_)
      + "', prec: " + std::to_string(precedence_) + ">\n";

    output += lhs_->to_string(n + 1);
    output += rhs_->to_string(n + 1);

    return output;
  }

  std::string ArrayType::to_string(size_t n) const {
    std::string output = tabs(n) + "<AraryType>\n";
    // NOTE: You can't ask about expr_type_ to determine the length
    // because it may not have been deduced yet.
    return output + array_type_->to_string(n + 1);
  }

  std::string ChainOp::to_string(size_t n) const {
    std::string output = tabs(n) + "<Chain: ";
    for (const auto& op : ops_) {
      output += op->token() + " ";
    }

    output += ", prec: " + std::to_string(precedence()) + ">\n";

    for (const auto& expr : exprs_) {
      output += expr->to_string(n + 1);
    }

    return output;
  }

  std::string Terminal::to_string(size_t n) const {
    return tabs(n) + "<Terminal " + expr_type_->to_string() + ": "
      + token_ + ">\n";
  }

  std::string Identifier::to_string(size_t n) const {
    return tabs(n) + "<Identifier " + expr_type_->to_string() + ": "
      + token() + ">\n";
  }

  std::string Declaration::to_string(size_t n) const {
    std::string output = tabs(n) + "<Declaration ";
    if (infer_type_) {
      output += "(infer type) ";
    }

    return output + expr_type_->to_string() + ">\n"
      + id_->to_string(n + 1)
      + decl_type_->to_string(n + 1);
  }

  std::string Assignment::to_string(size_t n) const {
    return tabs(n)
      + "<Assignment " + expr_type_->to_string() + ">\n"
      + lhs_->to_string(n + 1)
      + rhs_->to_string(n + 1);
  }

  std::string Case::to_string(size_t n) const {
    return tabs(n) + "<Case>\n" + pairs_->to_string(n + 1);
  }

  std::string KVPairList::to_string(size_t n) const {
    std::string indent = tabs(n);
    std::string output;

    size_t counter = 0;
    for (const auto& kv : kv_pairs_) {
      ++counter;
      output += indent + "[=> " + std::to_string(counter) + " of " + std::to_string(kv_pairs_.size()) + "]\n";
      output += kv.first->to_string(n + 1);
      output += kv.second->to_string(n + 1);
    }

    return output;
  }

  std::string Statements::to_string(size_t n) const {
    std::string output = tabs(n) + "<Statements>\n";

    for (const auto& exprs : statements_) {
      output += exprs->to_string(n + 1);
    }

    return output;
  }

  std::string FunctionLiteral::to_string(size_t n) const {
    std::string output = tabs(n) + "<FunctionLiteral>\n";
    for (const auto& kv : inputs_) {
      output += kv->to_string(n + 1);
    }
    return output + tabs(n + 1) + "Body:\n" + statements_->to_string(n + 2);
  }

}  // namespace AST
