#ifndef ICARUS_AST_BLOCK_NODE_H
#define ICARUS_AST_BLOCK_NODE_H

#include <memory>
#include "ast/expression.h"
#include "ast/statements.h"
#include "scope.h"

namespace ast {
struct BlockNode : public Expression {
  ~BlockNode() override {}

  // TODO
  template <typename... Args>
  BlockNode(Args &&... args) {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;


  std::unique_ptr<Expression> name_;
  Statements stmts_;
  std::unique_ptr<Expression> arg_; // TODO used?
  std::unique_ptr<ExecScope> block_scope_;
};
}  // namespace ast

#endif  // ICARUS_AST_BLOCK_NODE_H
