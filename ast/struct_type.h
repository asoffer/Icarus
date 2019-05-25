#ifndef ICARUS_AST_STRUCT_TYPE_H
#define ICARUS_AST_STRUCT_TYPE_H

#include "ast/expression.h"

namespace ast {
struct StructType : public Expression {
  StructType(TextSpan span) : Expression(span) {}
  ~StructType() override {}

#include "visitor/visitors.xmacro.h"

  std::vector<std::unique_ptr<Expression>> args_;
};
}  // namespace ast

#endif  // ICARUS_AST_STRUCT_TYPE_H
