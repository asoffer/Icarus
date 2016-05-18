#ifndef ICARUS_UNITY
#include "Type.h"
#endif

#define TYPE_OR(other) (type ? type->to_string() : (other))

namespace AST {
static std::string tabs(size_t n) { return std::string(n << 1, ' '); }

std::string TokenNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "[" << token << "]\n";
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
  assert(length);
  std::string output = tabs(n) + "<ArrayType>\n";
  return output + length->to_string(n + 1) + data_type->to_string(n + 1);
}

std::string ChainOp::to_string(size_t n) const {
  std::string output = tabs(n) + "<Chain: ";
  // TODO lookup table show token

  output += ", prec: " + std::to_string(precedence) + ">\n";

  for (const auto &expr : exprs) { output += expr->to_string(n + 1); }

  return output;
}

std::string Terminal::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<Terminal " << TYPE_OR("") << ": ";

  switch (terminal_type) {
  case Language::Terminal::ASCII: ss << "ascii"; break;
  case Language::Terminal::Char: ss << "'" << value.as_char << "'"; break;
  case Language::Terminal::Else: ss << "else"; break;
  case Language::Terminal::False: ss << "false"; break;
  case Language::Terminal::Hole: ss << "--"; break;
  case Language::Terminal::Int: ss << value.as_int; break;
  case Language::Terminal::Null: ss << "null"; break;
  case Language::Terminal::Ord: ss << "ord"; break;
  case Language::Terminal::Real: ss << value.as_real; break;
  case Language::Terminal::Return: ss << "return"; break;
  case Language::Terminal::StringLiteral: ss << value.as_str; break;
  case Language::Terminal::True: ss << "true"; break;
  case Language::Terminal::Type: ss << "type"; break; // TODO FIXME
  case Language::Terminal::Uint: ss << value.as_uint; break;
  }

  ss << ">\n";
  return ss.str();
}

std::string Identifier::to_string(size_t n) const {
  return tabs(n) + "<Identifier " + TYPE_OR("") + ": " + token + ">\n";
}

std::string InDecl::to_string(size_t n) const {
  return tabs(n) + "<"+ identifier->token + " in>\n" + container->to_string(n + 1);
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
  case DeclType::Tick: {
    output += "(`)";
  } break;
  }

  return output + TYPE_OR("") + ">\n" + identifier->to_string(n + 1) +
         expr->to_string(n + 1);
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

std::string ParametricStructLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<ParametricStruct>\n";
  for (const auto &decl : declarations) { ss << decl->to_string(n + 1); }

  return ss.str();
}
std::string StructLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<Struct>\n";
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
  return tabs(n) + "<" + value.as_type->to_string() + ">\n";
}
} // namespace AST
