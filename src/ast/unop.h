#ifndef ICARUS_AST_UNOP_H
#define ICARUS_AST_UNOP_H

#include "ast/dispatch.h"
#include "ast/expression.h"
#include "frontend/operators.h"

namespace AST {
struct Unop : public Expression {
  ~Unop() override {}

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

  Unop *Clone() const override;

  std::unique_ptr<Expression> operand;
  Language::Operator op;
  DispatchTable dispatch_table_;
};
} //
#endif  // ICARUS_AST_UNOP_H
