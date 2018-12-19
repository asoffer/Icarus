#ifndef ICARUS_AST_SWITCH_H
#define ICARUS_AST_SWITCH_H

#include "ast/expression.h"

namespace ast {
struct Switch : public Expression {
  ~Switch() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *rets) const override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  base::vector<
      std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>>
      cases_;
};
}  // namespace ast

#endif  // ICARUS_AST_SWITCH_H
