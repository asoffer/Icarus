#include "ast.h"

#define TYPE_OR(other) (type ? type->to_string() : (other))

namespace Language {
size_t precedence(Operator op);
} // Language

namespace AST {
static std::string tabs(size_t n) { return std::string(n << 1, ' '); }

std::string TokenNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "[" << token << "]\n";
  return ss.str();
}

std::string ArrayLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "[";
  auto iter = elems.begin();
  ss << (*iter)->to_string(n);
  ++iter;
  while (iter != elems.end()) {
    ss << ", " << (*iter)->to_string(n);
    ++iter;
  }
  ss << "]";
  return ss.str();
}

std::string Call::to_string(size_t n) const {
  std::stringstream ss;
  ss << fn_->to_string(n) << "(";
  bool seen_one = false;
  for (const auto &pos : args_.pos_) {
    ss << (seen_one ? ", " : "") << pos->to_string(n);
    seen_one = true;
  }
  for (const auto & [ key, val ] : args_.named_) {
    ss << (seen_one ? ", " : "") << key << " = " << val->to_string(n) << ", ";
    seen_one = true;
  }
  ss << ")";
  return ss.str();
}

std::string For::to_string(size_t n) const {
  std::stringstream ss;
  ss << "for ";
  auto iter = iterators.begin();
  ss << (*iter)->to_string(n);
  ++iter;
  while (iter != iterators.end()) {
    ss << ", " << (*iter)->to_string(n);
    ++iter;
  }
  ss << " {\n" << statements->to_string(n + 1) << tabs(n) << "}";
  return ss.str();
}

std::string Unop::to_string(size_t n) const {
  std::stringstream ss;
  switch (op) {
  case Language::Operator::Return: ss << "return "; break;
  case Language::Operator::Break: ss << "break "; break;
  case Language::Operator::Continue: ss << "continue "; break;
  case Language::Operator::Repeat: ss << "repeat "; break;
  case Language::Operator::Restart: ss << "restart "; break;
  case Language::Operator::Print: ss << "print "; break;
  case Language::Operator::Free: ss << "free "; break;
  case Language::Operator::Mul: ss << "*"; break;
  case Language::Operator::And: ss << "&"; break;
  case Language::Operator::Sub: ss << "-"; break;
  case Language::Operator::Generate: ss << "generate "; break;
  case Language::Operator::Not: ss << "!"; break;
  case Language::Operator::At: ss << "@"; break;
  case Language::Operator::Eval: ss << "$"; break;
  case Language::Operator::Dots: ss << ".."; break; // TODO
  case Language::Operator::Require: ss << "require "; break;
  case Language::Operator::Ref: ss << "\\"; break;
  case Language::Operator::Needs: ss << "needs "; break;
  case Language::Operator::Ensure: ss << "ensure "; break;
  case Language::Operator::TypeOf: ss << ":? "; break;
  case Language::Operator::Pass: break;
  default: { UNREACHABLE(); }
  }

  ss << operand->to_string(n);
  return ss.str();
}

std::string Access::to_string(size_t n) const {
  return operand->to_string(n) + "." + member_name;
}

std::string Binop::to_string(size_t n) const {
  std::stringstream ss;
  if (op == Language::Operator::Index) {
    ss << lhs->to_string(n) << "[" << rhs->to_string(n) << "]";
    return ss.str();
  }
  if (lhs->precedence < Language::precedence(op) || lhs->is<Declaration>()) {
    ss << "(" << lhs->to_string(n) << ")";
  } else {
    ss << lhs->to_string(n);
  }
  switch (op) {
  case Language::Operator::Cast: ss << "Cast"; break;
  case Language::Operator::Arrow: ss << " -> "; break;
  case Language::Operator::Add: ss << " + "; break;
  case Language::Operator::Sub: ss << " - "; break;
  case Language::Operator::Mul: ss << " * "; break;
  case Language::Operator::Div: ss << " / "; break;
  case Language::Operator::Mod: ss << " % "; break;
  case Language::Operator::Dots: ss << " .. "; break;
  case Language::Operator::Rocket: ss << "  =>  "; break;
  case Language::Operator::Assign: ss << ""; break;
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
  if (rhs->precedence < Language::precedence(op) || rhs->is<Declaration>()) {
    ss << "(" << rhs->to_string(n) << ")";
  } else {
    ss << rhs->to_string(n);
  }

  return ss.str();
}

std::string ArrayType::to_string(size_t n) const {
  ASSERT(length, "");
  std::stringstream ss;
  ss << "[" << length->to_string(n) << "; " << data_type->to_string(n) << "]";
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
  auto iter = exprs.begin();
  ss << (*iter)->to_string(n);
  ++iter;
  while (iter != exprs.end()) {
    ss << ", " << (*iter)->to_string(n);
    ++iter;
  }
  return ss.str();
}

std::string Terminal::to_string(size_t) const { return value.to_string(); }
std::string Identifier::to_string(size_t) const { return token; }

std::string InDecl::to_string(size_t n) const {
  return identifier->token + " in " + container->to_string(n);
}

std::string Declaration::to_string(size_t n) const {
  std::stringstream ss;
  ss << identifier->to_string(n);
  if (type_expr) {
    ss << (const_ ? " :: " : ": ") << type_expr->to_string(n);
    if (init_val) { ss << " = " << init_val->to_string(n); }
  } else {
    if (init_val) {
      ss << (const_ ? " ::= " : " := ") << init_val->to_string(n);
    }
  }

  return ss.str();
}

std::string Case::to_string(size_t n) const {
  std::stringstream ss;
  ss << "case {\n";
  for (const auto & [ key, val ] : key_vals) {
    ss << tabs(n + 1) << key->to_string(n + 1) << "  =>  "
       << val->to_string(n + 1) << "\n";
  }

  return ss.str();
}

std::string Statements::to_string(size_t n) const {
  if (content_.empty()) { return ""; }
  if (content_.size() == 1) { return tabs(n) + content_[0]->to_string(n); }

  std::stringstream ss;
  for (size_t i = 0; i < content_.size() - 1; ++i) {
    ss << tabs(n) << content_[i]->to_string(n) << "\n";
  }
  ss << tabs(n) << content_.back()->to_string(n);
  return ss.str();
}

std::string FunctionLiteral::to_string(size_t n) const {
  std::stringstream ss;
  if (inputs.empty()) {
    ss << "void";
  } else {
    auto iter = inputs.begin();
    ss << "(" << (*iter)->to_string(n);
    ++iter;
    while (iter != inputs.end()) {
      ss << ", " << (*iter)->to_string(n);
      ++iter;
    }
    ss << ")";
  }
  ss << " -> " << return_type_expr->to_string(n + 1) << " {\n"
     << statements->to_string(n + 1) << tabs(n) << "}";
  return ss.str();
}

std::string GenericFunctionLiteral::to_string(size_t n) const {
  return FunctionLiteral::to_string(n);
}

std::string Jump::to_string(size_t) const {
  switch (jump_type) {
  case JumpType::Restart: return "restart";
  case JumpType::Continue: return "continue";
  case JumpType::Repeat: return "repeat";
  case JumpType::Break: return "break";
  case JumpType::Return: return "return";
  default: UNREACHABLE();
  }
}

std::string ScopeNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << scope_expr->to_string(n);
  if (expr) { ss << " " << expr->to_string(n); }
  ss << " {\n" << stmts->to_string(n) << tabs(n) << "}";
  return ss.str();
}

std::string CodeBlock::to_string(size_t n) const {
  if (auto* err = std::get_if<std::string>(&content_)) {
    return "error(" + *err + ")";
  }
  auto str = std::get<Statements>(content_).to_string(n + 1);

  if (str.empty()) { return "{{}}"; }
  if (str.find('\n') != std::string::npos) { return "{{\n" + str + "\n}}"; }

  str[0] = '{';
  str[1] = ' ';
  return "{" + str + " }}";
}

std::string Hole::to_string(size_t) const { return "--"; }

std::string ScopeLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "scope {\n"
     << tabs(n + 1) << enter_fn->to_string(n + 1) << "\n"
     << tabs(n + 1) << exit_fn->to_string(n + 1) << "\n"
     << tabs(n) << "}";
  return ss.str();
}

std::string StructLiteral::to_string(size_t n) const { 
  std::stringstream ss;
  ss << "struct {\n";
  for (const auto &f : fields_) {
    ss << tabs(n + 1) << f->to_string(n) << "\n";
  }
  ss << tabs(n) << "}";
  return ss.str();
}
} // namespace AST
