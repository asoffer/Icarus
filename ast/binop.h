#ifndef ICARUS_AST_BINOP_H
#define ICARUS_AST_BINOP_H

#include <memory>
#include <vector>

#include "ast/expression.h"
#include "frontend/operators.h"

struct Context;

namespace ast {
struct Binop : public Expression {
  ~Binop() override {}

#include "visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    std::stringstream ss;
    ss << "(" << lhs->to_string(n) << ")";
    switch (op) {
      case frontend::Operator::Arrow: ss << " -> "; break;
      case frontend::Operator::Add: ss << " + "; break;
      case frontend::Operator::Sub: ss << " - "; break;
      case frontend::Operator::Mul: ss << " * "; break;
      case frontend::Operator::Div: ss << " / "; break;
      case frontend::Operator::Mod: ss << " % "; break;
      case frontend::Operator::Assign: ss << " = "; break;
      case frontend::Operator::OrEq: ss << " |= "; break;
      case frontend::Operator::XorEq: ss << " ^= "; break;
      case frontend::Operator::AndEq: ss << " &= "; break;
      case frontend::Operator::AddEq: ss << " += "; break;
      case frontend::Operator::SubEq: ss << " -= "; break;
      case frontend::Operator::MulEq: ss << " *= "; break;
      case frontend::Operator::DivEq: ss << " /= "; break;
      case frontend::Operator::ModEq: ss << " %= "; break;
      case frontend::Operator::When: ss << " when "; break;
      default: UNREACHABLE();
    }
    ss << "(" << rhs->to_string(n) << ")";

    return ss.str();
  }

  frontend::Operator op;
  std::unique_ptr<Expression> lhs, rhs;
};

}  // namespace ast

#endif  // ICARUS_AST_BINOP_H
