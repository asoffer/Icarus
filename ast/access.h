#ifndef ICARUS_AST_ACCESS_H
#define ICARUS_AST_ACCESS_H

#include <string>
#include "ast/expression.h"

namespace ast {
struct Access : public Expression {
  ~Access() override {}
  std::string to_string(size_t n) const override {
    return operand->to_string(n) + "." + member_name;
  }

#include "ast_visitor/visitors.xmacro.h"

  void DependentDecls(DeclDepGraph *g,
                      Declaration *d) const override;
  bool InferType(type::Type const *t, InferenceState *state) const override;

  ir::Results EmitIr(Context *) override;
  std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  std::string member_name;
  std::unique_ptr<Expression> operand;
};

}  // namespace ast

#endif  // ICARUS_AST_ACCESS_H
