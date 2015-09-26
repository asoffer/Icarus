#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include <string>
#include <iostream>
#include <map>
#include "typedefs.h"
#include "AST/Node.h"

namespace AST {
  extern std::map<std::string, size_t> prec_map;

  class Expression : public Node {
    public:
      static constexpr size_t prec_max = 100;

      static NPtr from_identifier(std::vector<NPtr>&& nodes);
      static NPtr parenthesize(std::vector<NPtr>&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual ~Expression(){}

    protected:
      Expression() {}

      size_t precedence_;
  };

  inline NPtr Expression::from_identifier(std::vector<NPtr>&& nodes) {
    auto expr_ptr = new Expression;
    expr_ptr->type_ = expression;
    expr_ptr->token_ = nodes[0]->token();
    expr_ptr->precedence_ = prec_max;

    return std::unique_ptr<Node>(expr_ptr);
  }

  inline NPtr Expression::parenthesize(std::vector<NPtr>&& nodes) {
    auto expr_ptr = static_cast<Expression*>(nodes[1].release());
    expr_ptr->precedence_ = prec_max;
    expr_ptr->type_ = paren_expression;

    return std::unique_ptr<Node>(expr_ptr);
  }

  class Binop : public Expression {
    public:
      static NPtr build(std::vector<NPtr>&& nodes);

      virtual std::string to_string(size_t n) const;

      virtual ~Binop(){}

    private:
      Binop() {}
      std::unique_ptr<Expression> lhs_;
      std::unique_ptr<Expression> rhs_;
  };

  inline NPtr Binop::build(std::vector<NPtr>&& nodes) {
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ =
      std::unique_ptr<Expression>(static_cast<Expression*>(nodes[0].release()));

    binop_ptr->rhs_ =
      std::unique_ptr<Expression>(static_cast<Expression*>(nodes[2].release()));

    binop_ptr->token_ = nodes[1]->token();
    binop_ptr->type_ = operat;
    binop_ptr->precedence_ = prec_map["*"];

    return std::unique_ptr<Node>(binop_ptr);
  }

}  // namespace AST

#endif  // ICARUS_AST_EXPRESSION_H
