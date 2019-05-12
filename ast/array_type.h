#ifndef ICARUS_AST_ARRAY_TYPE_H
#define ICARUS_AST_ARRAY_TYPE_H

#include "ast/expression.h"

namespace ast {
struct ArrayType : public Expression {
  ~ArrayType() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  std::unique_ptr<Expression> length_, data_type_;
};
}  // namespace ast

#endif  // ICARUS_AST_ARRAY_TYPE_H
