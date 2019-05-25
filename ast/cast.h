#ifndef ICARUS_AST_CAST_H
#define ICARUS_AST_CAST_H

#include <memory>
#include <vector>

#include "ast/expression.h"

struct Context;

namespace ast {
struct Cast : public Expression {
  ~Cast() override {}

#include "visitor/visitors.xmacro.h"

  std::unique_ptr<Expression> expr_, type_;
};

}  // namespace ast

#endif  // ICARUS_AST_CAST_H
