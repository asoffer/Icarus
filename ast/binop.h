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

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  frontend::Operator op;
  std::unique_ptr<Expression> lhs, rhs;
};

}  // namespace ast

#endif  // ICARUS_AST_BINOP_H
