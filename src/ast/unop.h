#ifndef ICARUS_AST_UNOP_H
#define ICARUS_AST_UNOP_H

#include "ast/dispatch.h"
#include "ast/expression.h"
#include "frontend/operators.h"

namespace ast {
struct Unop : public Expression {
  ~Unop() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  bool needs_expansion() const override {
    return !parenthesized_ && op == Language::Operator::Expand;
  }

  std::unique_ptr<Expression> operand;
  Language::Operator op;
  DispatchTable dispatch_table_;
};
}  // namespace ast
#endif  // ICARUS_AST_UNOP_H
