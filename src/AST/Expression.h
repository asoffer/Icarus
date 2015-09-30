#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include <iostream>
#include "typedefs.h"
#include "AST/Node.h"
#include "AST/Scope.h"

namespace AST {
  class Expression : public Node {
    friend class Binop;

    public:
      static NPtr from_identifier(NPtrVec&& nodes);
      static NPtr parenthesize(NPtrVec&& nodes);

      size_t precedence() const;

      virtual void join_identifiers(Scope* scope) = 0;

      virtual bool is_identifier() const { return false; }
      virtual bool is_binop() const { return false; }

      virtual ~Expression(){}

    protected:
      Expression() {}

      size_t precedence_;
  };

  inline NPtr Expression::parenthesize(NPtrVec&& nodes) {
    auto expr_ptr = static_cast<Expression*>(nodes[1].release());
    expr_ptr->precedence_ = Language::op_prec.at("MAX");
    return NPtr(expr_ptr);
  }

  inline size_t Expression::precedence() const {
    return precedence_;
  }

}  // namespace AST

#endif  // ICARUS_AST_EXPRESSION_H
