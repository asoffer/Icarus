#ifndef ICARUS_AST_BINOP_H
#define ICARUS_AST_BINOP_H

#include "Expression.h"
#include "AST/Node.h"
#include "AST/Terminal.h"
#include "typedefs.h"

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

      virtual std::set<std::string> identifiers() const;
      virtual void verify_no_declarations() const;
      virtual std::string to_string(size_t n) const;
      virtual void separate_declarations_and_assignments();
      virtual bool is_binop() { return true; }


      virtual ~Binop(){}

    private:
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
    else_ptr->base_type_ = Node::reserved_else;
    else_ptr->token_ = "else";
    else_ptr->precedence_ = prec_max;

    binop_ptr->lhs_ = EPtr(else_ptr);

    binop_ptr->rhs_ =
      EPtr(static_cast<Expression*>(nodes[2].release()));

    binop_ptr->token_ = "=>";
    binop_ptr->type_ = generic_operator;
    binop_ptr->precedence_ = prec_map["=>"];

    return NPtr(binop_ptr);
  }

  inline NPtr Binop::build_operator(NPtrVec&& nodes, std::string op_symbol) {
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ =
      EPtr(static_cast<Expression*>(nodes[0].release()));

    binop_ptr->rhs_ =
      EPtr(static_cast<Expression*>(nodes[2].release()));

    binop_ptr->token_ = op_symbol;
    binop_ptr->type_ = generic_operator;
    binop_ptr->precedence_ = prec_map[op_symbol];

    return NPtr(binop_ptr);
  }

  inline std::set<std::string> Binop::identifiers() const {
    std::set<std::string> lhs_set = lhs_->identifiers();
    std::set<std::string> rhs_set = rhs_->identifiers();
    lhs_set.insert(rhs_set.begin(), rhs_set.end());

    return lhs_set;
  }
}  // namespace AST

#endif  // ICARUS_AST_BINOP_H
