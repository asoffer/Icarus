#ifndef ICARUS_AST_SWITCH_H
#define ICARUS_AST_SWITCH_H

#include "ast/expression.h"

namespace ast {
// TODO consider separating this into two classes given that we know when we
// parse if it has parens or not.
struct Switch : public Expression {
  ~Switch() override {}

#include "visitor/visitors.xmacro.h"

  std::unique_ptr<Expression> expr_;
  std::vector<std::pair<std::unique_ptr<Node>, std::unique_ptr<Expression>>>
      cases_;
};

}  // namespace ast

#endif  // ICARUS_AST_SWITCH_H
