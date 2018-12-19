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

  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;

  void ExtractJumps(JumpExprs *rets) const override {
    operand->ExtractJumps(rets);
  }

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  std::string member_name;
  std::unique_ptr<Expression> operand;
};

}  // namespace ast

#endif  // ICARUS_AST_ACCESS_H
