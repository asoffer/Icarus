#include "Type.h"

#define TYPE_OR(other) (type ? type->to_string() : (other))

namespace AST {
std::string tabs(size_t n) { return std::string(n << 1, ' '); }

std::string Node::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) + "[";
  switch (type_) {
  case Language::unknown: ss << "Unknown"; break;
  case Language::bof: ss << "BOF"; break;
  case Language::eof: ss << "EOF"; break;
  case Language::newline: ss << "Newline"; break;
  case Language::comment: ss << "Comment"; break;
  case Language::semicolon: ss << ";"; break;
  case Language::hashtag: ss << "#..."; break;
  case Language::op_b: ss << "Binop"; break;
  case Language::op_bl: ss << "Binop or Left Unop"; break;
  case Language::op_l: ss << "Left Unop"; break;
  case Language::op_lt: ss << "Left Unop or Terminal"; break;
  case Language::expr: ss << "Expression"; break;
  case Language::fn_arrow: ss << "->"; break;
  case Language::l_paren: ss << "Left Paren"; break;
  case Language::r_paren: ss << "Right Paren"; break;
  case Language::l_brace: ss << "Left Brace"; break;
  case Language::r_brace: ss << "Right Brace"; break;
  case Language::l_bracket: ss << "Left Bracket"; break;
  case Language::r_bracket: ss << "Right Bracket"; break;
  default: ss << "???"; break;
  }

  ss << (!token_.empty() ? ": " + token_ : "") << "]\n";
  return ss.str();
}

std::string Conditional::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<Conditional (" << statements.size() << " part"
     << (statements.size() == 1 ? "" : "s") << ")>\n";

  for (size_t i = 0; i < conditions.size(); ++i) {
    ss << tabs(n + 1) << "Condition " << i << ":\n"
       << conditions[i]->to_string(n + 1) << statements[i]->to_string(n + 1);
  }

  if (has_else()) {
    ss << tabs(n + 1) << "Else:\n" << statements.back()->to_string(n + 1);
  }

  return ss.str();
}

std::string ArrayLiteral::to_string(size_t n) const {
  std::string output = tabs(n) + "<ArrayLiteral>\n";
  for (const auto &el : elems) { output += el->to_string(n + 1); }

  return output;
}

std::string While::to_string(size_t n) const {
  return tabs(n) + "<While>\n" + condition->to_string(n + 1) +
         statements->to_string(n + 1);
}

std::string For::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<For>\n";
  for (auto iter : iterators) { ss << iter->to_string(n + 1); }
  ss << statements->to_string(n + 1);
  return ss.str();
}

std::string Unop::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<Unop " << TYPE_OR("") << ": ";
  switch (op) {
  case Language::Operator::Return: ss << "Return"; break;
  case Language::Operator::Break: ss << "Break"; break;
  case Language::Operator::Continue: ss << "Continue"; break;
  case Language::Operator::Repeat: ss << "Repeat"; break;
  case Language::Operator::Restart: ss << "Restart"; break;
  case Language::Operator::Print: ss << "Print"; break;
  case Language::Operator::Free: ss << "Free"; break;
  case Language::Operator::And: ss << "And"; break;
  case Language::Operator::Sub: ss << "Sub"; break;
  case Language::Operator::Not: ss << "Not"; break;
  case Language::Operator::At: ss << "At"; break;
  case Language::Operator::Call: ss << "Call"; break;
  case Language::Operator::Dots: ss << "Dots"; break;
  case Language::Operator::Import: ss << "Import"; break;

  default: { assert(false && "Not a unary operator"); }
  }
  ss << ">\n" << operand->to_string(n + 1);
  return ss.str();
}

std::string Access::to_string(size_t n) const {
  return tabs(n) + "<Access " + member_name + " " + TYPE_OR("") + ">\n" +
         operand->to_string(n + 1);
}

std::string Binop::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n);
  if (is_assignment()) {
    ss << "<Assignment " << TYPE_OR("");

  } else {
    ss << "<Binop " << TYPE_OR("") << ": ";
    switch (op) {
    case Language::Operator::Cast: ss << "Cast"; break;
    case Language::Operator::Arrow: ss << "->"; break;
    case Language::Operator::Or: ss << "Or"; break;
    case Language::Operator::Xor: ss << "Xor"; break;
    case Language::Operator::And: ss << "And"; break;
    case Language::Operator::Add: ss << "Add"; break;
    case Language::Operator::Sub: ss << "Sub"; break;
    case Language::Operator::Mul: ss << "Mul"; break;
    case Language::Operator::Div: ss << "Div"; break;
    case Language::Operator::Mod: ss << "Mod"; break;
    case Language::Operator::Index: ss << "Index"; break;
    case Language::Operator::Call: ss << "Call"; break;
    case Language::Operator::Dots: ss << "Dots"; break;
    case Language::Operator::In: ss << "In"; break;
    case Language::Operator::Tick: ss << "Tick"; break;
    case Language::Operator::Rocket: ss << "Rocket"; break;
    default: assert(false && "Not a binary operator");
    }
  }
  ss << ">\n" << lhs->to_string(n + 1) << rhs->to_string(n + 1);
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

  for (const auto &expr : exprs) { output += expr->to_string(n + 1); }

  return output;
}

std::string Terminal::to_string(size_t n) const {
  auto str = tabs(n) + "<Terminal " + TYPE_OR("") + ": ";

  if (token_ == "\n")
    str += "\\n";
  else if (token_ == "\t")
    str += "\\t";
  else if (token_ == "\r")
    str += "\\r";
  else if (token_ == " ")
    str += "' '";
  else
    str += token_;
  return str + ">\n";
}

std::string Identifier::to_string(size_t n) const {
  return tabs(n) + "<Identifier " + TYPE_OR("") + ": " + token() + ">\n";
}

std::string Declaration::to_string(size_t n) const {
  std::string output = tabs(n) + "<Declaration ";
  switch (decl_type) {
  case DeclType::Std: {
    output += "(:)";
  } break;
  case DeclType::Infer: {
    output += "(:=)";
  } break;
  case DeclType::In: {
    output += "(in)";
  } break;
  case DeclType::Tick: {
    output += "(`)";
  } break;
  }

  return output + TYPE_OR("") + ">\n" + identifier->to_string(n + 1) +
         type_expr->to_string(n + 1);
}

std::string Case::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<Case>\n";
  auto indent = tabs(n + 1);

  size_t counter      = 0;
  size_t num_key_vals = key_vals.size();
  for (const auto &kv : key_vals) {
    ++counter;
    ss << indent << "[=> " << std::to_string(counter) << " of "
       << std::to_string(num_key_vals) << "]\n" << kv.first->to_string(n + 1)
       << kv.second->to_string(n + 1);
  }
  return ss.str();
}

std::string Statements::to_string(size_t n) const {
  std::string output = tabs(n) + "<Statements>\n";

  for (const auto &exprs : statements) { output += exprs->to_string(n + 1); }

  return output;
}

std::string FunctionLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<FunctionLiteral>\n";
  for (const auto &kv : inputs) { ss << kv->to_string(n + 1); }
  ss << tabs(n + 1) << "Body:\n" << statements->to_string(n + 2);
  return ss.str();
}

std::string StructLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<Type>\n";
  for (const auto &decl : declarations) { ss << decl->to_string(n + 1); }

  return ss.str();
}

std::string EnumLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<Enum with " << members.size()
     << (members.size() == 1 ? " value>\n" : " values>\n");
  return ss.str();
}

std::string Jump::to_string(size_t n) const {
  switch (jump_type) {
  case JumpType::Restart: return tabs(n) + "<Restart>\n";
  case JumpType::Continue: return tabs(n) + "<Continue>\n";
  case JumpType::Repeat: return tabs(n) + "<Repeat>\n";
  case JumpType::Break: return tabs(n) + "<Break>\n";
  case JumpType::Return: return tabs(n) + "<Return>\n";
  }
}

std::string DummyTypeExpr::to_string(size_t n) const {
  return tabs(n) + "<" + type_value->to_string() + ">\n";
}
} // namespace AST
