#ifndef ICARUS_AST_ARRAY_LITERAL_H
#define ICARUS_AST_ARRAY_LITERAL_H

#include "ast/comma_list.h"
#include "ast/expression.h"

namespace ast {
struct ArrayLiteral : public Expression {
  ArrayLiteral(TextSpan const &span) : Expression(span) {}
  ~ArrayLiteral() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    std::stringstream ss;
    ss << "[";
    if (!cl_.exprs_.empty()) {
      auto iter = cl_.exprs_.begin();
      ss << (*iter)->to_string(n);
      ++iter;
      while (iter != cl_.exprs_.end()) {
        ss << ", " << (*iter)->to_string(n);
        ++iter;
      }
    }
    ss << "]";
    return ss.str();
  }

  CommaList cl_;
};
}  // namespace ast

#endif  // ICARUS_AST_ARRAY_LITERAL_H
