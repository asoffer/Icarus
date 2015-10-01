#ifndef ICARUS_AST_DECLARATION_H
#define ICARUS_AST_DECLARATION_H

#include "typedefs.h"
#include "Language.h"
#include "AST/Binop.h"

namespace AST {
  class Declaration : public Binop {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;

      virtual ~Declaration(){}

    private:
      Declaration() {}
  };

  inline NPtr Declaration::build(NPtrVec&& nodes) {
    auto decl_ptr = new Declaration;
    decl_ptr->lhs_ =
      EPtr(static_cast<Expression*>(nodes[0].release()));

    decl_ptr->rhs_ =
      EPtr(static_cast<Expression*>(nodes[2].release()));

    decl_ptr->token_ = ":";
    decl_ptr->type_ = Language::decl_operator;

    decl_ptr->precedence_ = Language::op_prec.at(":");

    return NPtr(decl_ptr);
  }
}  // namespace AST

#endif  // ICARUS_AST_DECLARATION_H
