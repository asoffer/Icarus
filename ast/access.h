#ifndef ICARUS_AST_ACCESS_H
#define ICARUS_AST_ACCESS_H

#include <string>
#include "ast/expression.h"

namespace ast {
struct Access : public Expression {
  ~Access() override {}
  std::string to_string(size_t n) const override {
    return operand->to_string(n) + "." + member_name;
  }

#include "ast_visitor/visitors.xmacro.h"

  std::string member_name;
  std::unique_ptr<Expression> operand;
};

}  // namespace ast

#endif  // ICARUS_AST_ACCESS_H
