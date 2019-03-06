#ifndef ICARUS_AST_CHAINOP_H
#define ICARUS_AST_CHAINOP_H

#include <memory>
#include <vector>
#include "ast/literal.h"
#include "frontend/operators.h"

namespace ast {
struct ChainOp : public Literal {
  ~ChainOp() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(core::Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;

  ir::Results EmitIr(Context *) override;

  std::vector<frontend::Operator> ops;
  std::vector<std::unique_ptr<Expression>> exprs;
};
}  // namespace ast
#endif  // ICARUS_AST_CHAINOP_H
