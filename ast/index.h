#ifndef ICARUS_AST_INDEX_H
#define ICARUS_AST_INDEX_H

#include <memory>
#include <vector>

#include "ast/expression.h"

struct Context;

namespace ast {
struct Index : public Expression {
  ~Index() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  std::unique_ptr<Expression> lhs_, rhs_;
};

}  // namespace ast

#endif  // ICARUS_AST_INDEX_H
