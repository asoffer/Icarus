#ifndef ICARUS_AST_BINOP_H
#define ICARUS_AST_BINOP_H

#include "AST/Node.h"
#include "AST/Expression.h"
#include "AST/Terminal.h"
#include "typedefs.h"
#include "Language.h"

namespace AST {
  class Binop : public Expression {
    public:
      // It's not clear it's okay to pass the string by reference if it's
      // referencing something in the node. Probably not worth trying because
      // this is highly unlikely to be a bottleneck.
      static NPtr build_operator(NPtrVec&& nodes, std::string op_symbol);

      static NPtr build(NPtrVec&& nodes);
      static NPtr build_else_kv(NPtrVec&& nodes);
      static NPtr build_paren_operator(NPtrVec&& nodes);
      static NPtr build_bracket_operator(NPtrVec&& nodes);

      virtual void join_identifiers(Scope* scope);
      virtual std::string to_string(size_t n) const;
      virtual bool is_binop() const { return true; }


      virtual ~Binop(){}

    protected:
      Binop() {}
      EPtr lhs_;
      EPtr rhs_;
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

  inline NPtr Binop::build_else_kv(NPtrVec&& nodes) {
    auto binop_ptr = new Binop;
    auto else_ptr = new Terminal;
    else_ptr->base_type_ = Language::reserved_else;
    else_ptr->token_ = "else";
    else_ptr->precedence_ = Language::op_prec.at("MAX");

    binop_ptr->lhs_ = EPtr(else_ptr);

    binop_ptr->rhs_ =
      EPtr(static_cast<Expression*>(nodes[2].release()));

    binop_ptr->token_ = "=>";
    binop_ptr->type_ = Language::generic_operator;

    return NPtr(binop_ptr);
  }

  inline NPtr Binop::build_operator(NPtrVec&& nodes, std::string op_symbol) {
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ =
      EPtr(static_cast<Expression*>(nodes[0].release()));

    binop_ptr->rhs_ =
      EPtr(static_cast<Expression*>(nodes[2].release()));

    binop_ptr->token_ = op_symbol;
    binop_ptr->type_ = Language::generic_operator;

    binop_ptr->precedence_ = Language::op_prec.at(op_symbol);

    return NPtr(binop_ptr);
  }
}  // namespace AST

#endif  // ICARUS_AST_BINOP_H
