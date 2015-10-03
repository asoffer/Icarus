#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include <iostream>
#include "typedefs.h"
#include "AST/Node.h"
#include "AST/Scope.h"
#include "Type.h"

namespace AST {
  class Expression : public Node {
    friend class Binop;
    friend class Declaration;
    friend class Assignment;
    friend class Scope;

    public:
      static NPtr parenthesize(NPtrVec&& nodes);

      size_t precedence() const { return precedence_; }

      virtual void join_identifiers(Scope* scope) = 0;
      virtual void verify_types() {}
      virtual void find_all_decls(Scope*) {}

      virtual ~Expression(){}

    protected:
      Expression() {}

      size_t precedence_;
      Type expr_type_;
  };

  inline NPtr Expression::parenthesize(NPtrVec&& nodes) {
    auto expr_ptr = static_cast<Expression*>(nodes[1].release());
    expr_ptr->precedence_ = Language::op_prec.at("MAX");
    return NPtr(expr_ptr);
  }

}  // namespace AST

#endif  // ICARUS_AST_EXPRESSION_H
