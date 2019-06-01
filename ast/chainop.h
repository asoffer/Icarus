#ifndef ICARUS_AST_CHAINOP_H
#define ICARUS_AST_CHAINOP_H

#include <memory>
#include <vector>

#include "ast/expression.h"
#include "frontend/operators.h"

namespace ast {
struct ChainOp : public Expression {
  ~ChainOp() override {}

#include "visitor/visitors.xmacro.h"

  std::vector<std::unique_ptr<Expression>> &&extract() && {
    return std::move(exprs);
  }

  std::vector<frontend::Operator> ops;
  std::vector<std::unique_ptr<Expression>> exprs;
};
}  // namespace ast
#endif  // ICARUS_AST_CHAINOP_H
