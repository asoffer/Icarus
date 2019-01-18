#ifndef ICARUS_AST_CHAINOP_H
#define ICARUS_AST_CHAINOP_H

#include <memory>
#include "ast/dispatch.h"
#include "ast/expression.h"
#include "base/container/vector.h"
#include "frontend/operators.h"

namespace ast {
struct ChainOp : public Expression {
  ~ChainOp() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  base::vector<Language::Operator> ops;
  base::vector<std::unique_ptr<Expression>> exprs;
};
}  // namespace ast
#endif  // ICARUS_AST_CHAINOP_H
