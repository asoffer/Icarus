#ifndef ICARUS_AST_TERMINAL_H
#define ICARUS_AST_TERMINAL_H

#include <set>
#include "AST/Expression.h"
#include "AST/Node.h"
#include "typedefs.h"
#include "Language.h"

namespace AST {
  class Terminal : public Expression {
    friend class Binop;

    public:
      static NPtr build(NPtrVec&& nodes, Language::NodeType t);
      static NPtr build_string_literal(NPtrVec&& nodes);
      static NPtr build_integer(NPtrVec&& nodes);
      static NPtr build_real(NPtrVec&& nodes);

      virtual std::set<std::string> identifiers() const;
      virtual void verify_no_declarations() const;
      virtual std::string to_string(size_t n) const;
      virtual void separate_declarations_and_assignments(){}

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

  inline std::set<std::string> Terminal::identifiers() const {
    return std::set<std::string>();
  }

  inline void Terminal::verify_no_declarations() const {}

}  // namespace AST
#endif  // ICARUS_AST_TERMINAL_H
