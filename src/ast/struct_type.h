#ifndef ICARUS_AST_STRUCT_TYPE_H
#define ICARUS_AST_STRUCT_TYPE_H

#include "ast/expression.h"

namespace ast {
struct StructType : public Expression {
  StructType(TextSpan span) : Expression(span) {}
  ~StructType() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *ct) override;

  std::vector<std::unique_ptr<Expression>> args_;
};
}  // namespace ast

#endif  // ICARUS_AST_STRUCT_TYPE_H
