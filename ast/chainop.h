#ifndef ICARUS_AST_CHAINOP_H
#define ICARUS_AST_CHAINOP_H

#include <memory>
#include <vector>

#include "ast/expression.h"
#include "frontend/operators.h"

namespace ast {
struct ChainOp : public Expression {
  ~ChainOp() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override{
    std::stringstream ss;
    ss << "(";
    for (size_t i = 0; i < ops.size(); ++i) {
      ss << exprs[i]->to_string(n);
      switch (ops[i]) {
        case frontend::Operator::Or: ss << " | "; break;
        case frontend::Operator::Xor: ss << " ^ "; break;
        case frontend::Operator::And: ss << " & "; break;
        case frontend::Operator::Lt: ss << " < "; break;
        case frontend::Operator::Le: ss << " <= "; break;
        case frontend::Operator::Eq: ss << " == "; break;
        case frontend::Operator::Ne: ss << " != "; break;
        case frontend::Operator::Ge: ss << " >= "; break;
        case frontend::Operator::Gt: ss << " > "; break;
        default: UNREACHABLE();
      }
    }
    ss << exprs.back()->to_string(n) << ")";
    return ss.str();
  }

  std::vector<frontend::Operator> ops;
  std::vector<std::unique_ptr<Expression>> exprs;
};
}  // namespace ast
#endif  // ICARUS_AST_CHAINOP_H
