#ifndef ICARUS_AST_TERMINAL_H
#define ICARUS_AST_TERMINAL_H

#include "Expression.h"
#include "Node.h"
#include "typedefs.h"

namespace AST {
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

}  // namespace AST
#endif  // ICARUS_AST_TERMINAL_H
