#ifndef ICARUS_AST_ARRAY_LITERAL_H
#define ICARUS_AST_ARRAY_LITERAL_H

#include "ast/comma_list.h"
#include "ast/expression.h"

namespace ast {
struct ArrayLiteral : public Expression {
  ArrayLiteral(TextSpan const &span) : Expression(span) {}
  ~ArrayLiteral() override {}

#include "visitor/visitors.xmacro.h"

  CommaList cl_;
};
}  // namespace ast

#endif  // ICARUS_AST_ARRAY_LITERAL_H
