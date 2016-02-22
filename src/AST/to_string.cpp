#include "AST.h"
#include "Type.h"
#include <sstream>

#define TYPE_OR(other) (type ? type->to_string() : (other))

namespace AST {
std::string tabs(size_t n) { return std::string(n << 1, ' '); }

std::string Node::to_string(size_t n) const {
  std::string output = tabs(n) + "[";
  switch (type_) {
    case Language::unknown:                 output += "Unknown";       break;
    case Language::eof:                     output += "EOF";           break;
    case Language::newline:                 output += "Newline";       break;
    case Language::comment:                 output += "Comment";       break;
    case Language::identifier:              output += "Identifier";    break;
    case Language::int_literal:             output += "Integer";       break;
    case Language::uint_literal:            output += "UInt";          break;
    case Language::real_literal:            output += "Real";          break;
    case Language::type_literal:            output += "Type";          break;
    case Language::char_literal:            output += "Character";     break;
    case Language::string_literal:          output += "String";        break;
    case Language::generic_operator:        output += "Operator";      break;
    case Language::bool_operator:           output += "BoolOperator";  break;
    case Language::dot:                     output += "Dot";           break;
    case Language::binary_boolean_operator: output += "BinOperator";   break;
    case Language::decl_operator:           output += ":";             break;
    case Language::decl_assign_operator:    output += ":=";            break;
    case Language::assign_operator:         output += "X=";            break;
    case Language::fn_arrow:                output += "->";            break;
    case Language::comma:                   output += ":";             break;
    case Language::semicolon:               output += ";";             break;
    case Language::dereference:             output += "@";             break;
    case Language::negation:                output += "-";             break;
    case Language::indirection:             output += "&";             break;
    case Language::rocket_operator:         output += "=>";            break;
    case Language::key_value_pair:          output += "( => )";        break;
    case Language::expression:              output += "Expression";    break;
    case Language::left_paren:              output += "Left Paren";    break;
    case Language::right_paren:             output += "Right Paren";   break;
    case Language::left_brace:              output += "Left Brace";    break;
    case Language::right_brace:             output += "Right Brace";   break;
    case Language::left_bracket:            output += "Left Bracket";  break;
    case Language::right_bracket:           output += "Right Bracket"; break;
#define RESERVED_MACRO(res)                                                    \
    case Language::reserved_##res:          output += #res;            break;
#include "config/reserved.conf"
#undef RESERVED_MACRO
    default:;
  }

  return output + (!token_.empty() ? ": " + token_ : "") + "]\n";
}

std::string Conditional::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n)
    << "<Conditional (" << statements_.size() << " part"
      << (statements_.size() == 1 ? "" : "s")
      << ")>\n";

    for (size_t i = 0; i < conds_.size(); ++i) {
      ss << tabs(n + 1) << "Condition " << i << ":\n"
        << conds_[i]->to_string(n + 1) << statements_[i]->to_string(n + 1);
    }

    if (has_else()) {
      ss << tabs(n + 1) << "Else:\n" << statements_.back()->to_string(n + 1);
    }

    return ss.str();
  }


  std::string ArrayLiteral::to_string(size_t n) const {
    std::string output = tabs(n) + "<ArrayLiteral>\n";
    for (const auto& el : elems) {
      output += el->to_string(n + 1);
    }

    return output;
  }

  std::string While::to_string(size_t n) const {
    return tabs(n) + "<While>\n" + cond_->to_string(n + 1)
      + statements_->to_string(n + 1);
  }

  std::string Unop::to_string(size_t n) const {
    std::stringstream ss;
    ss << tabs(n) << "<Unop " << TYPE_OR("") << ": ";
    switch (op) {
      case Language::Operator::Return: ss << "Return"; break;
      case Language::Operator::Print:  ss << "Print";  break;
      case Language::Operator::Free:   ss << "Free";   break;
      case Language::Operator::And:    ss << "And";    break;
      case Language::Operator::Sub:    ss << "Sub";    break;
      case Language::Operator::Not:    ss << "Not";    break;
      case Language::Operator::At:     ss << "At";     break;
      case Language::Operator::Call:   ss << "Call";   break;
      default: assert(false && "Not a unary operator");
    }
    ss << ">\n" << operand->to_string(n + 1);
    return ss.str();
  }

  std::string Access::to_string(size_t n) const {
    return tabs(n) + "<Access " + member_name + " " + TYPE_OR("") + ">\n"
      + operand->to_string(n + 1);
  }

  std::string Binop::to_string(size_t n) const {
    std::stringstream ss;
    ss << tabs(n) << "<Binop " << TYPE_OR("") << ": ";
    switch (op) {
      case Language::Operator::Cast:    ss << "Cast";   break;
      case Language::Operator::Arrow:   ss << "->";     break;
      case Language::Operator::Or:      ss << "Or";     break;
      case Language::Operator::Xor:     ss << "Xor";    break;
      case Language::Operator::And:     ss << "And";    break;
      case Language::Operator::Add:     ss << "Add";    break;
      case Language::Operator::Sub:     ss << "Sub";    break;
      case Language::Operator::Mul:     ss << "Mul";    break;
      case Language::Operator::Div:     ss << "Div";    break;
      case Language::Operator::Mod:     ss << "Mod";    break;
      case Language::Operator::Index:   ss << "Index";  break;
      case Language::Operator::Call:    ss << "Call";   break;
      default: assert(false && "Not a binary operator");
    }
    ss << ">\n"
      << lhs->to_string(n + 1)
      << rhs->to_string(n + 1);
    return ss.str();
  }

  std::string ArrayType::to_string(size_t n) const {
    std::string output = tabs(n) + "<ArrayType>\n";
    return output + data_type->to_string(n + 1);
  }

  std::string ChainOp::to_string(size_t n) const {
    std::string output = tabs(n) + "<Chain: ";
    // TODO lookup table show token
    // for (const auto& op : ops) {
    //   output += op->token() + " ";
    // }

    output += ", prec: " + std::to_string(precedence) + ">\n";

    for (const auto& expr : exprs) {
      output += expr->to_string(n + 1);
    }

    return output;
  }

  std::string Terminal::to_string(size_t n) const {
    auto str =  tabs(n) + "<Terminal " + TYPE_OR("") + ": ";

    if (token_ == "\n")       str += "\\n";
    else if (token_ == "\t")  str += "\\t";
    else if (token_ == "\r")  str += "\\r";
    else if (token_ == " ")   str += "' '";
    else str += token_;
    return str + ">\n";
  }

  std::string Identifier::to_string(size_t n) const {
    return tabs(n) + "<Identifier " + TYPE_OR("") + ": "
      + token() + ">\n";
  }

  std::string Declaration::to_string(size_t n) const {
    std::string output = tabs(n) + "<Declaration ";
    if (is_inferred) output += "(infer type) ";

    return output + TYPE_OR("") + ">\n"
      + identifier->to_string(n + 1)
      + type_expr->to_string(n + 1);
  }

  std::string Assignment::to_string(size_t n) const {
    return tabs(n) + "<Assignment " + TYPE_OR("") + ">\n"
      + lhs->to_string(n + 1)
      + rhs->to_string(n + 1);
  }

  std::string Case::to_string(size_t n) const {
    return tabs(n) + "<Case>\n" + kv->to_string(n + 1);
  }

  std::string KVPairList::to_string(size_t n) const {
    std::string indent = tabs(n);
    std::string output;

    size_t counter = 0;
    for (const auto& kv : pairs) {
      ++counter;
      output += indent + "[=> " + std::to_string(counter) + " of " + std::to_string(pairs.size()) + "]\n";
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
    std::stringstream ss;
    ss << tabs(n) << "<FunctionLiteral>\n";
    for (const auto& kv : inputs_) {
      ss << kv->to_string(n + 1);
    }
    ss << tabs(n + 1) << "Body:\n" << statements_->to_string(n + 2);
    return ss.str();
  }

  std::string TypeLiteral::to_string(size_t n) const {
    std::stringstream ss;
    ss << tabs(n) << "<Type>\n";
    for (const auto& decl : decls_) {
      ss << decl->to_string(n + 1);
    }

    return ss.str();
  }

  std::string EnumLiteral::to_string(size_t n) const {
    std::stringstream ss;
    ss << tabs(n) << "<Enum with " << vals_.size()
      << (vals_.size() == 1 ? " value>\n" : " values>\n");
    return ss.str();
  }
  std::string Break::to_string(size_t n) const {
    return tabs(n) + "<Break>";
  }

}  // namespace AST
