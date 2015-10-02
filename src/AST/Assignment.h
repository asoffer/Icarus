#ifndef ICARUS_AST_ASSIGNMENT_H
#define ICARUS_AST_ASSIGNMENT_H

#include "typedefs.h"
#include "Language.h"
#include "AST/Binop.h"

namespace AST {
  class Assignment : public Binop {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;

      virtual void verify_types();

      virtual ~Assignment(){}

    private:
      Assignment() {}
  };

  inline NPtr Assignment::build(NPtrVec&& nodes) {
    auto assign_ptr = new Assignment;
    assign_ptr->lhs_ =
      EPtr(static_cast<Expression*>(nodes[0].release()));

    assign_ptr->rhs_ =
      EPtr(static_cast<Expression*>(nodes[2].release()));

    assign_ptr->token_ = ":";
    assign_ptr->type_ = Language::assign_operator;

    assign_ptr->precedence_ = Language::op_prec.at("=");

    return NPtr(assign_ptr);
  }
}  // namespace AST

#endif  // ICARUS_AST_ASSIGNMENT_H
