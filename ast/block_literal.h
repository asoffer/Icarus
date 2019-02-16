#ifndef ICARUS_AST_BLOCK_LITERAL_H
#define ICARUS_AST_BLOCK_LITERAL_H

#include "ast/literal.h"

namespace ast {
struct BlockLiteral : public Literal {
  BlockLiteral(bool required);
  ~BlockLiteral() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;

  std::vector<ir::Val> EmitIR(Context *) override;

  std::vector<std::unique_ptr<Expression>> before_, after_;
  std::unique_ptr<Scope> body_scope_;
  bool required_;
};
}  // namespace ast

#endif  // ICARUS_AST_BLOCK_LITERAL_H
