#ifndef ICARUS_AST_ARRAY_TYPE_H
#define ICARUS_AST_ARRAY_TYPE_H

#include "ast/literal.h"

namespace ast {
struct ArrayType : public Literal {
  ~ArrayType() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;

  std::vector<ir::Val> EmitIR(Context *) override;

  std::unique_ptr<Expression> length_, data_type_;
};
}  // namespace ast

#endif  // ICARUS_AST_ARRAY_TYPE_H
