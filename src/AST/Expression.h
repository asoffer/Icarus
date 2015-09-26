#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include <string>
#include <iostream>
#include <map>
#include <utility>
#include "typedefs.h"
#include "AST/Node.h"

namespace AST {
  extern std::map<std::string, size_t> prec_map;

  class Expression : public Node {
    friend class Binop;

    public:
      static constexpr size_t prec_max = 100;

      static NPtr from_identifier(NPtrVec&& nodes);
      static NPtr parenthesize(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual ~Expression(){}

    protected:
      Expression() {}

      size_t precedence_;
  };

  inline NPtr Expression::parenthesize(NPtrVec&& nodes) {
    auto expr_ptr = static_cast<Expression*>(nodes[1].release());
    expr_ptr->precedence_ = prec_max;
    expr_ptr->type_ = paren_expression;

    return NPtr(expr_ptr);
  }

  class Terminal : public Expression {
    public:
      static NPtr build(NPtrVec&& nodes, Node::Type t);
      static NPtr build_identifier(NPtrVec&& nodes);
      static NPtr build_integer(NPtrVec&& nodes);
      static NPtr build_real(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;

    private:
      Node::Type base_type_;
      Terminal() {}
  };

  inline NPtr Terminal::build(NPtrVec&& nodes, Node::Type t) {
    auto term_ptr = new Terminal;
    term_ptr->base_type_ = t;
    term_ptr->type_ = expression;
    term_ptr->token_ = nodes[0]->token();
    term_ptr->precedence_ = prec_max;

    return NPtr(term_ptr);
  }

  inline NPtr Terminal::build_identifier(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), identifier);
  }

  inline NPtr Terminal::build_integer(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), integer);
  }

  inline NPtr Terminal::build_real(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), real);
  }


  class Binop : public Expression {
    public:
      static NPtr build(NPtrVec&& nodes);

      NPtr fix_tree_precedence(bool return_ptr);

      virtual std::string to_string(size_t n) const;

      virtual ~Binop(){}

    private:
      Binop() {}
      std::unique_ptr<Expression> lhs_;
      std::unique_ptr<Expression> rhs_;
  };

  inline NPtr Binop::build(NPtrVec&& nodes) {
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ =
      std::unique_ptr<Expression>(static_cast<Expression*>(nodes[0].release()));

    binop_ptr->rhs_ =
      std::unique_ptr<Expression>(static_cast<Expression*>(nodes[2].release()));

    binop_ptr->token_ = nodes[1]->token();
    binop_ptr->type_ = operat;
    binop_ptr->precedence_ = prec_map[nodes[1]->token()];

    return binop_ptr->fix_tree_precedence(true);
  }

}  // namespace AST

#endif  // ICARUS_AST_EXPRESSION_H
