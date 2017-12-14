#include "../type/type.h"
#include "ast.h"
#include "../ir/ir.h"

#define TYPE_OR(other) (type ? type->to_string() : (other))

extern std::string Escape(char c);

namespace AST {
static std::string tabs(size_t n) { return std::string(n << 1, ' '); }

std::string TokenNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "[" << token << "]\n";
  return ss.str();
}

std::string ArrayLiteral::to_string(size_t n) const {
  std::string output = tabs(n) + "<ArrayLiteral " + TYPE_OR("") + " >\n ";
  for (const auto &el : elems) { output += el->to_string(n + 1); }

  return output;
}

std::string Call::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<Call " << TYPE_OR("") << ">\n" << fn_->to_string(n + 1);
  for (const auto &pos : pos_) { ss << pos->to_string(n + 1); }
  for (const auto &kv : named_) {
    ss << tabs(n + 1) << kv.first << " =>\n" << kv.second->to_string(n + 2);
  }
  return ss.str();
}

std::string For::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<For>\n";
  for (auto& iter : iterators) { ss << iter->to_string(n + 1); }
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
  case Language::Operator::Mul: ss << "Mul"; break;
  case Language::Operator::And: ss << "And"; break;
  case Language::Operator::Sub: ss << "Sub"; break;
  case Language::Operator::Generate: ss << "Generate"; break;
  case Language::Operator::Not: ss << "Not"; break;
  case Language::Operator::At: ss << "At"; break;
  case Language::Operator::Eval: ss << "Eval"; break;
  case Language::Operator::Dots: ss << "Dots"; break;
  case Language::Operator::Require: ss << "Require"; break;
  case Language::Operator::Ref: ss << "Ref"; break;
  case Language::Operator::Needs: ss << "Needs"; break;
  case Language::Operator::Ensure: ss << "Ensure"; break;
  default: { UNREACHABLE(); }
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
    case Language::Operator::Dots: ss << "Dots"; break;
    case Language::Operator::In: ss << "In"; break;
    case Language::Operator::Tick: ss << "Tick"; break;
    case Language::Operator::Rocket: ss << "Rocket"; break;
    default: UNREACHABLE();
    }
  }
  ss << ">\n"
     << lhs->to_string(n + 1)
     << (rhs ? rhs->to_string(n + 1) : tabs(n + 1) + "0x0\n");
  return ss.str();
}

std::string ArrayType::to_string(size_t n) const {
  ASSERT(length, "");
  std::string output = tabs(n) + "<ArrayType>\n";
  return output + length->to_string(n + 1) + data_type->to_string(n + 1);
}

std::string ChainOp::to_string(size_t n) const {
  std::string output = tabs(n) + "<Chain: " + TYPE_OR("") + ">\n";
  for (auto& expr : exprs) { output += expr->to_string(n + 1); }
  return output;
}

std::string CommaList::to_string(size_t n) const {
  std::string output = tabs(n) + "<CommaList: " + TYPE_OR("") + ">\n";
  for (auto& expr : exprs) { output += expr->to_string(n + 1); }
  return output;
}

std::string Terminal::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<Terminal " << TYPE_OR("") << ": " << value.to_string()
     << ">\n";
  return ss.str();
}

std::string Identifier::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<Identifier " << TYPE_OR("") << ": \"" << token << "\">\n";
  return ss.str();
}

std::string InDecl::to_string(size_t n) const {
  return tabs(n) + "<" + TYPE_OR("") + " " + identifier->token + " in>\n" +
         container->to_string(n + 1);
}

std::string Declaration::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<Declaration " << (IsInferred() ? "(:=)" : "(:)")
     << TYPE_OR("") << ">\n" << identifier->to_string(n + 1);

  if (type_expr) { ss << type_expr->to_string(n + 1); }
  if (init_val) { ss << init_val->to_string(n + 1); }
  return ss.str();
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
  std::stringstream ss;
  ss << tabs(n) << "<Statements>\n";

  for (const auto &exprs : statements) { ss << exprs->to_string(n + 1); }

  return ss.str();
}

std::string FunctionLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<FunctionLiteral " << TYPE_OR("") << ">\n";
  ss << tabs(n + 1) << "Inputs\n";
  for (const auto &kv : inputs) { ss << kv->to_string(n + 2); }
  ss << tabs(n + 1) << "Outputs\n";
  ss << return_type_expr->to_string(n + 1) << tabs(n + 1) << "Body:\n"
     << statements->to_string(n + 2);
  return ss.str();
}

std::string Jump::to_string(size_t n) const {
  switch (jump_type) {
  case JumpType::Restart: return tabs(n) + "<Restart>\n";
  case JumpType::Continue: return tabs(n) + "<Continue>\n";
  case JumpType::Repeat: return tabs(n) + "<Repeat>\n";
  case JumpType::Break: return tabs(n) + "<Break>\n";
  case JumpType::Return: return tabs(n) + "<Return>\n";
  default: UNREACHABLE();
  }
}

std::string ScopeNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<ScopeNode>\n";
  ss << scope_expr->to_string(n + 1);
  if (expr) { ss << expr->to_string(n + 1); }
  ss << tabs(n + 1) << "Statements:\n";
  ss << stmts->to_string(n + 2);
  return ss.str();
}

std::string CodeBlock::to_string(size_t n) const { return tabs(n) + "<...>\n"; }

std::string ScopeLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << tabs(n) << "<ScopeLiteral " << TYPE_OR("") << ">\n"
     << enter_fn->to_string(n + 2) << exit_fn->to_string(n + 2);
  return ss.str();
}
} // namespace AST
