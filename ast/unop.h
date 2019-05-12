#ifndef ICARUS_AST_UNOP_H
#define ICARUS_AST_UNOP_H

#include "ast/expression.h"
#include "frontend/operators.h"

namespace ast {
struct Unop : public Expression {
  ~Unop() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    if (op == frontend::Operator::TypeOf) {
      return "(" + operand->to_string(n) + "):?";
    }

    std::stringstream ss;
    switch (op) {
      case frontend::Operator::Which: ss << "which "; break;
      case frontend::Operator::Mul: ss << "*"; break;
      case frontend::Operator::And: ss << "&"; break;
      case frontend::Operator::Sub: ss << "-"; break;
      case frontend::Operator::Not: ss << "!"; break;
      case frontend::Operator::At: ss << "@"; break;
      case frontend::Operator::Eval: ss << "$"; break;
      case frontend::Operator::Needs: ss << "needs "; break;
      case frontend::Operator::Ensure: ss << "ensure "; break;
      case frontend::Operator::Expand: ss << "<< "; break;
      case frontend::Operator::BufPtr: ss << "[*]"; break;
      case frontend::Operator::Copy: ss << "copy "; break;
      case frontend::Operator::Move: ss << "move "; break;
      default: { UNREACHABLE(); }
    }

    ss << operand->to_string(n);
    return ss.str();
  }

  bool needs_expansion() const override {
    return !parenthesized_ && op == frontend::Operator::Expand;
  }

  std::unique_ptr<Expression> operand;
  frontend::Operator op;
};
}  // namespace ast
#endif  // ICARUS_AST_UNOP_H
