#ifndef ICARUS_AST_COMMA_LIST_H
#define ICARUS_AST_COMMA_LIST_H

#include "ast/expression.h"

namespace AST {
struct CommaList : public Expression {
  CommaList() = default;
  ~CommaList() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  void VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, std::vector<IR::Val> *args) override;
  void ExtractReturns(std::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) override;

  CommaList *Clone() const override;
  IR::Val EmitIR(Context *) override;
  IR::Val EmitLVal(Context *) override;

  std::vector<std::unique_ptr<Expression>> exprs;
};
}  // namespace AST

#endif // ICARUS_AST_COMMA_LIST_H
