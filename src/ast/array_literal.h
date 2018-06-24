#ifndef ICARUS_AST_ARRAY_LITERAL_H
#define ICARUS_AST_ARRAY_LITERAL_H

#include "ast/expression.h"

namespace AST {
struct ArrayLiteral : public Expression {
  ~ArrayLiteral() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  void VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, std::vector<IR::Val> *args) override;
  void ExtractReturns(std::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) override;

  std::vector<IR::Val> EmitIR(Context *) override;
  std::vector<IR::Val> EmitLVal(Context *) override;
  ArrayLiteral *Clone() const override;

  // TODO replace with a comma-list
  std::vector<std::unique_ptr<Expression>> elems_;
};
}  // namespace AST

#endif  // ICARUS_AST_ARRAY_LITERAL_H

