#ifndef ICARUS_AST_ARRAY_LITERAL_H
#define ICARUS_AST_ARRAY_LITERAL_H

#include "ast/expression.h"

namespace ast {
struct ArrayLiteral : public Expression {
  ~ArrayLiteral() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::Register> EmitLVal(Context *) override;

  // TODO replace with a comma-list
  base::vector<std::unique_ptr<Expression>> elems_;
};
}  // namespace ast

#endif  // ICARUS_AST_ARRAY_LITERAL_H
