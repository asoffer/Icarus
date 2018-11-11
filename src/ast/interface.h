#ifndef ICARUS_AST_INTERFACE_H
#define ICARUS_AST_INTERFACE_H

#include "ast/declaration.h"
#include "ast/expression.h"
#include "scope.h"

namespace ast {
struct Interface : public Expression {
  ~Interface() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::Register> EmitLVal(Context *) override;

  base::vector<Declaration> decls_;
  std::unique_ptr<DeclScope> body_scope_;
};
}  // namespace ast

#endif  // ICARUS_AST_INTERFACE_H
