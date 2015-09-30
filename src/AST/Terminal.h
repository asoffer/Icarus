#ifndef ICARUS_AST_TERMINAL_H
#define ICARUS_AST_TERMINAL_H

#include "AST/Node.h"
#include "AST/Expression.h"
#include "typedefs.h"
#include "Language.h"

namespace AST {
  class Scope;

  class Terminal : public Expression {
    friend class Binop;

    public:
      static NPtr build(NPtrVec&& nodes, Language::NodeType t);
      static NPtr build_string_literal(NPtrVec&& nodes);
      static NPtr build_integer(NPtrVec&& nodes);
      static NPtr build_real(NPtrVec&& nodes);

      virtual void join_identifiers(Scope*) {}

      virtual std::string to_string(size_t n) const;

    protected:
      Language::NodeType base_type_;
      Terminal() {}
  };

  inline NPtr Terminal::build(NPtrVec&& nodes, Language::NodeType t) {
    auto term_ptr = new Terminal;
    term_ptr->base_type_ = t;
    term_ptr->token_ = nodes[0]->token();
    term_ptr->precedence_ = Language::op_prec.at("MAX");

    return NPtr(term_ptr);
  }

  inline NPtr Terminal::build_string_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Language::string_literal);
  }

  inline NPtr Terminal::build_integer(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Language::integer);
  }

  inline NPtr Terminal::build_real(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Language::real);
  }

}  // namespace AST
#endif  // ICARUS_AST_TERMINAL_H
