#ifndef ICARUS_AST_COMMA_LIST_H
#define ICARUS_AST_COMMA_LIST_H

#include "ast/expression.h"

namespace AST {
struct CommaList : public Expression {
  CommaList() = default;
  CommaList(CommaList&&) noexcept = default;
  ~CommaList() override {}

  CommaList &operator=(CommaList &&) noexcept = default;

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, base::vector<IR::Val> *args) override;
  void ExtractReturns(base::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const base::unordered_map<const Expression *, IR::Val> &) override;

  CommaList *Clone() const override;
  base::vector<IR::Val> EmitIR(Context *) override;
  base::vector<IR::Register> EmitLVal(Context *) override;

  base::vector<std::unique_ptr<Expression>> exprs;
};
}  // namespace AST

#endif // ICARUS_AST_COMMA_LIST_H
