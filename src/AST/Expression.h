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
      // It's not clear it's okay to pass the string by reference if it's
      // referencing something in the node. Probably not worth trying because
      // this is highly unlikely to be a bottleneck.
      static NPtr build_operator(NPtrVec&& nodes, std::string op_symbol);

      static NPtr build(NPtrVec&& nodes);
      static NPtr build_paren_operator(NPtrVec&& nodes);
      static NPtr build_bracket_operator(NPtrVec&& nodes);

      NPtr fix_tree_precedence(bool return_ptr);

      virtual std::string to_string(size_t n) const;

      virtual ~Binop(){}

    private:
      Binop() {}
      std::unique_ptr<Expression> lhs_;
      std::unique_ptr<Expression> rhs_;
  };

  inline NPtr Binop::build_paren_operator(NPtrVec&& nodes) {
    return Binop::build_operator(std::forward<NPtrVec>(nodes), "()");
  }

  inline NPtr Binop::build_bracket_operator(NPtrVec&& nodes) {
    return Binop::build_operator(std::forward<NPtrVec>(nodes), "[]");
  }

  inline NPtr Binop::build(NPtrVec&& nodes) {
    return Binop::build_operator(std::forward<NPtrVec>(nodes), nodes[1]->token());
  }

  inline NPtr Binop::build_operator(NPtrVec&& nodes, std::string op_symbol) {
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ =
      std::unique_ptr<Expression>(static_cast<Expression*>(nodes[0].release()));

    binop_ptr->rhs_ =
      std::unique_ptr<Expression>(static_cast<Expression*>(nodes[2].release()));

    binop_ptr->token_ = op_symbol;
    binop_ptr->type_ = operat;
    binop_ptr->precedence_ = prec_map[op_symbol];

    return binop_ptr->fix_tree_precedence(true);
  }

}  // namespace AST

#endif  // ICARUS_AST_EXPRESSION_H
