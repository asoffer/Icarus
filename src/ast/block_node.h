#ifndef ICARUS_AST_BLOCK_NODE_H
#define ICARUS_AST_BLOCK_NODE_H

#include <memory>
#include "ast/expression.h"
#include "ast/statements.h"
#include "scope.h"

namespace AST {
struct BlockNode : public Expression {
  ~BlockNode() override {}

  // TODO
  template <typename... Args>
  BlockNode(Args &&... args) {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *) override;


  std::unique_ptr<Expression> name_;
  Statements stmts_;
  std::unique_ptr<Expression> arg_; // TODO used?
  std::unique_ptr<ExecScope> block_scope_;
};
}  // namespace AST

#endif  // ICARUS_AST_BLOCK_NODE_H
