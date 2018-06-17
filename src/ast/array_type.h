#ifndef ICARUS_AST_ARRAY_TYPE_H
#define ICARUS_AST_ARRAY_TYPE_H

#include "ast/expression.h"

namespace AST {
struct ArrayType : public Expression {
  ~ArrayType() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  void VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, std::vector<IR::Val> *args) override;
  void ExtractReturns(std::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) override;

  IR::Val EmitIR(Context *) override;
  IR::Val EmitLVal(Context *ct) override;

  ArrayType *Clone() const override;

  std::unique_ptr<Expression> length_, data_type_;
};
}  // namespace AST

#endif  // ICARUS_AST_ARRAY_TYPE_H
