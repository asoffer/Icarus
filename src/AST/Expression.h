#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include <string>
#include <iostream>
#include <map>
#include <set>
#include <utility>
#include "typedefs.h"
#include "AST/Node.h"

namespace AST {
  class Expression : public Node {
    friend class Binop;

    public:
      static NPtr from_identifier(NPtrVec&& nodes);
      static NPtr parenthesize(NPtrVec&& nodes);

      size_t precedence() const;

      virtual void verify_no_declarations() const = 0;
      virtual std::set<std::string> identifiers() const = 0;

      virtual void separate_declarations_and_assignments() = 0;

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
