#include "AST.h"

namespace AST {
  std::string Node::graphviz_label() const { return "Generic node"; }
  std::string Conditional::graphviz_label() const { return "If"; }
  std::string While::graphviz_label() const { return "While"; }

  std::string Unop::graphviz_label() const {
    switch (op) {
      case Language::Operator::Return: return "Return";
      case Language::Operator::Print:  return "Print";
      case Language::Operator::Free:   return "Free";
      case Language::Operator::And:    return "And";
      case Language::Operator::Sub:    return "Sub";
      case Language::Operator::Not:    return "Not";
      case Language::Operator::At:     return "At";
      case Language::Operator::Call:   return "Call";
      default: assert(false && "Not a unary operator");
    }
  }

  std::string Identifier::graphviz_label() const {
    return "ID: " + token();
  }

  std::string Declaration::graphviz_label() const {
    return identifier->token() + (is_inferred ? " := ..." : " : ...");
  }

  std::string Access::graphviz_label() const { return "." + member_name; }

  std::string Terminal::graphviz_label() const {
    std::string output = "Terminal: ";
    if (type == Char) {
      output += "'";
      if (token_ == "\n")       output += "\\\\n";
      else if (token_ == "\t")  output += "\\\\t";
      else if (token_ == "\r")  output += "\\\\r";
      else output += token_;
      output += "'";
    } else {
      output += token_;
    }
    return output;
  }

  std::string ChainOp::graphviz_label() const {
    std::string output;
    for (const auto& op : ops) {
      switch (op) {
        case Language::Operator::Or:          output += "| ";  break;
        case Language::Operator::Xor:         output += "^ ";  break;
        case Language::Operator::And:         output += "& ";  break;
        case Language::Operator::LessThan:    output += "< ";  break;
        case Language::Operator::LessEq:      output += "<= "; break;
        case Language::Operator::Equal:       output += "== "; break;
        case Language::Operator::NotEqual:    output += "!= "; break;
        case Language::Operator::GreaterEq:   output += "<= "; break;
        case Language::Operator::GreaterThan: output += "< ";  break;
        case Language::Operator::Comma:       output += ", ";  break;
        default: assert(false && "Not a chain operator");
      }
    }
    return output;
  }

  std::string Assignment::graphviz_label() const {
    switch (op) {
      case Language::Operator::OrEq:  return "|=";
      case Language::Operator::XorEq: return "^=";
      case Language::Operator::AndEq: return "&=";
      case Language::Operator::AddEq: return "+=";
      case Language::Operator::SubEq: return "-=";
      case Language::Operator::MulEq: return "*=";
      case Language::Operator::DivEq: return "/=";
      case Language::Operator::ModEq: return "%=";
      case Language::Operator::Assign: return "=";
      default: assert(false && "Not an assignment operator");
    } 
  }

  std::string Binop::graphviz_label() const {
    switch (op) {
      case Language::Operator::Index:   return "Index";
      case Language::Operator::Call:    return "Call";
      case Language::Operator::Arrow:   return "->";
      case Language::Operator::Cast:    return ":>";
      case Language::Operator::Add:     return "+";
      case Language::Operator::Sub:     return "-";
      case Language::Operator::Mul:     return "*";
      case Language::Operator::Div:     return "/";
      case Language::Operator::Mod:     return "%";
      case Language::Operator::Rocket:  return "=>";
      default: assert(false && "Not an assignment operator");
    }
  }

  std::string TypeLiteral::graphviz_label() const {
    return type_value_ ? "Type: " + type_value_->name_ : "TypeLiteral";
  }

  std::string ArrayType::graphviz_label() const { return "ArrayType"; }
  std::string ArrayLiteral::graphviz_label() const {
    return "ArrayLiteral";
  }
  std::string Case::graphviz_label() const { return "Case"; }
  std::string KVPairList::graphviz_label() const { return "KVPairList"; }
  std::string Statements::graphviz_label() const { return "Statements"; }
  std::string FunctionLiteral::graphviz_label() const { return "FunctionLiteral"; }
  std::string EnumLiteral::graphviz_label() const { return "Enum"; }
  std::string Break::graphviz_label() const { return "Break"; }
}  // namespace AST

