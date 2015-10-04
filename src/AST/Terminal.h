#ifndef ICARUS_AST_TERMINAL_H
#define ICARUS_AST_TERMINAL_H

#include "AST/Node.h"
#include "AST/Expression.h"
#include "typedefs.h"
#include "Language.h"

namespace AST {
  class Scope;

  class Terminal : public Expression {
    friend class KVPairList;

    public:
      static NPtr build(NPtrVec&& nodes, Type t);
      static NPtr build_type_literal(NPtrVec&& nodes);
      static NPtr build_string_literal(NPtrVec&& nodes);
      static NPtr build_integer(NPtrVec&& nodes);
      static NPtr build_real(NPtrVec&& nodes);

      virtual void join_identifiers(Scope*) {}
      virtual void verify_types();


      virtual std::string to_string(size_t n) const;

    protected:
      Terminal() {}
  };

  inline NPtr Terminal::build(NPtrVec&& nodes, Type t) {
    auto term_ptr = new Terminal;
    term_ptr->expr_type_ = t;
    term_ptr->token_ = nodes[0]->token();
    term_ptr->precedence_ = Language::op_prec.at("MAX");

    return NPtr(term_ptr);
  }

  inline NPtr Terminal::build_type_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::Type_);
  }

  inline NPtr Terminal::build_string_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::String);
  }

  inline NPtr Terminal::build_integer(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::Int);
  }

  inline NPtr Terminal::build_real(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::Real);
  }

}  // namespace AST
#endif  // ICARUS_AST_TERMINAL_H
