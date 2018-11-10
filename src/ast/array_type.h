#ifndef ICARUS_AST_ARRAY_TYPE_H
#define ICARUS_AST_ARRAY_TYPE_H

#include "ast/expression.h"

namespace AST {
struct ArrayType : public Expression {
  ~ArrayType() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *ct) override;

  std::unique_ptr<Expression> length_, data_type_;
};
}  // namespace AST

#endif  // ICARUS_AST_ARRAY_TYPE_H
