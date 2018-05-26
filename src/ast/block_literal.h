#ifndef ICARUS_AST_BLOCK_LITERAL_H
#define ICARUS_AST_BLOCK_LITERAL_H

#include "ast/expression.h"

namespace AST {
struct BlockLiteral : public Expression {
  BlockLiteral(bool required);
  ~BlockLiteral() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  void ClearIdDecls() override;
  void VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, std::vector<IR::Val> *args) override;
  void ExtractReturns(std::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) override;

  IR::Val EmitIR(Context *) override;
  IR::Val EmitLVal(Context *) override;
  BlockLiteral *Clone() const override;

  std::unique_ptr<Expression> before_, after_;
  std::unique_ptr<Scope> body_scope_;
};
}  // namespace AST

#endif  // ICARUS_AST_BLOCK_LITERAL_H
