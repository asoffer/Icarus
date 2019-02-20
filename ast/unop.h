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
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;
  bool InferType(type::Type const *t, InferenceState *state) const override;

  std::vector<ir::Val> EmitIR(Context *) override;
  std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;
  void EmitMoveInit(type::Typed<ir::Register> reg, Context *ctx) override;
  void EmitCopyInit(type::Typed<ir::Register> reg, Context *ctx) override;

  bool needs_expansion() const override {
    return !parenthesized_ && op == frontend::Operator::Expand;
  }

  std::unique_ptr<Expression> operand;
  frontend::Operator op;
};
}  // namespace ast
#endif  // ICARUS_AST_UNOP_H
