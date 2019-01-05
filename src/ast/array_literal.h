#ifndef ICARUS_AST_ARRAY_LITERAL_H
#define ICARUS_AST_ARRAY_LITERAL_H

#include "ast/comma_list.h"

namespace ast {
struct ArrayLiteral : public Expression {
  ArrayLiteral() = default;
  ~ArrayLiteral() override {}

  void assign_scope(Scope *scope) override { return cl_.assign_scope(scope); }
  std::string to_string(size_t n) const override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context * ctx) override { return cl_.Validate(ctx); }
  void ExtractJumps(JumpExprs *rets) const override {
    return cl_.ExtractJumps(rets);
  }

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  CommaList cl_;
};
}  // namespace ast

#endif  // ICARUS_AST_ARRAY_LITERAL_H
