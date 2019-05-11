#ifndef ICARUS_AST_UNOP_H
#define ICARUS_AST_UNOP_H

#include "ast/expression.h"
#include "frontend/operators.h"

namespace ast {
struct Unop : public Expression {
  ~Unop() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;
  void DependentDecls(DeclDepGraph *g,
                      Declaration *d) const override;
  bool InferType(type::Type const *t, InferenceState *state) const override;

  ir::Results EmitIr(Context *) override;
  std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;
  void EmitMoveInit(type::Typed<ir::Reg> reg, Context *ctx) override;
  void EmitCopyInit(type::Typed<ir::Reg> reg, Context *ctx) override;

  bool needs_expansion() const override {
    return !parenthesized_ && op == frontend::Operator::Expand;
  }

  std::unique_ptr<Expression> operand;
  frontend::Operator op;
};
}  // namespace ast
#endif  // ICARUS_AST_UNOP_H
