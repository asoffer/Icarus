#ifndef ICARUS_AST_SWITCH_H
#define ICARUS_AST_SWITCH_H

#include "ast/expression.h"

namespace AST {
struct Switch : public Expression {
  ~Switch() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  void VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, base::vector<IR::Val> *args) override;
  void ExtractReturns(base::vector<const Expression *> *rets) const override;
  void contextualize(
      const Node *correspondant,
      const base::unordered_map<const Expression *, IR::Val> &) override;

  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *) override;

  Switch *Clone() const override;

  base::vector<
      std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>>
      cases_;
};
}  // namespace AST

#endif // ICARUS_AST_SWITCH_H
