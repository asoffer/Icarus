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

      size_t precedence() const;

      virtual ~Expression(){}

    protected:
      Expression() {}

      size_t precedence_;
  };

  inline NPtr Expression::parenthesize(NPtrVec&& nodes) {
    auto expr_ptr = static_cast<Expression*>(nodes[1].release());
    expr_ptr->precedence_ = prec_max;
    return NPtr(expr_ptr);
  }

  inline size_t Expression::precedence() const {
    return precedence_;
  }

}  // namespace AST

#endif  // ICARUS_AST_EXPRESSION_H
