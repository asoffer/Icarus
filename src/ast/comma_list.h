#ifndef ICARUS_AST_COMMA_LIST_H
#define ICARUS_AST_COMMA_LIST_H

#include "ast/expression.h"

namespace ast {
struct CommaList : public Expression {
  CommaList()                      = default;
  CommaList(CommaList &&) noexcept = default;
  ~CommaList() override {}

  CommaList &operator=(CommaList &&) noexcept = default;

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::Register> EmitLVal(Context *) override;

  base::vector<std::unique_ptr<Expression>> exprs;
};
}  // namespace ast

#endif  // ICARUS_AST_COMMA_LIST_H
