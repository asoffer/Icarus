#include "ast.h"

namespace AST {
std::string Access::to_string(size_t n) const {
  return operand->to_string(n) + "." + member_name;
}

std::string Binop::to_string(size_t n) const {
  std::stringstream ss;
  if (op == Language::Operator::Index) {
    ss << lhs->to_string(n) << "[" << rhs->to_string(n) << "]";
    return ss.str();
  }

  ss << "(" << lhs->to_string(n) << ")";
  switch (op) {
  case Language::Operator::Arrow: ss << " -> "; break;
  case Language::Operator::Add: ss << " + "; break;
  case Language::Operator::Sub: ss << " - "; break;
  case Language::Operator::Mul: ss << " * "; break;
  case Language::Operator::Div: ss << " / "; break;
  case Language::Operator::Mod: ss << " % "; break;
  case Language::Operator::Dots: ss << " .. "; break;
  case Language::Operator::Assign: ss << " <<:=>> "; break;
  case Language::Operator::OrEq: ss << " |= "; break;
  case Language::Operator::XorEq: ss << " ^= "; break;
  case Language::Operator::AndEq: ss << " &= "; break;
  case Language::Operator::AddEq: ss << " += "; break;
  case Language::Operator::SubEq: ss << " -= "; break;
  case Language::Operator::MulEq: ss << " *= "; break;
  case Language::Operator::DivEq: ss << " /= "; break;
  case Language::Operator::ModEq: ss << " %= "; break;
  default: UNREACHABLE();
  }
  ss << "(" << rhs->to_string(n) << ")";

  return ss.str();
}

std::string ChainOp::to_string(size_t n) const {
  std::stringstream ss;
  ss << "(";
  for (size_t i = 0; i < ops.size(); ++i) {
    ss << exprs[i]->to_string(n);
    switch (ops[i]) {
    case Language::Operator::Or: ss << " | "; break;
    case Language::Operator::Xor: ss << " ^ "; break;
    case Language::Operator::And: ss << " & "; break;
    case Language::Operator::Lt: ss << " < "; break;
    case Language::Operator::Le: ss << " <= "; break;
    case Language::Operator::Eq: ss << " == "; break;
    case Language::Operator::Ne: ss << " != "; break;
    case Language::Operator::Ge: ss << " >= "; break;
    case Language::Operator::Gt: ss << " > "; break;
    default: UNREACHABLE();
    }
  }
  ss << exprs.back()->to_string(n) << ")";
  return ss.str();
}

std::string CommaList::to_string(size_t n) const {
  std::stringstream ss;
  if (exprs.empty()) { return "()"; }
  auto iter = exprs.begin();
  ss << (*iter)->to_string(n);
  ++iter;
  while (iter != exprs.end()) {
    ss << ", " << (*iter)->to_string(n);
    ++iter;
  }
  return ss.str();
}

} // namespace AST
