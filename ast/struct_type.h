#ifndef ICARUS_AST_STRUCT_TYPE_H
#define ICARUS_AST_STRUCT_TYPE_H

#include "ast/expression.h"

namespace ast {
struct StructType : public Expression {
  StructType(TextSpan span) : Expression(span) {}
  ~StructType() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  std::vector<std::unique_ptr<Expression>> args_;
};
}  // namespace ast

#endif  // ICARUS_AST_STRUCT_TYPE_H
