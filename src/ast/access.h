#ifndef ICARUS_AST_ACCESS_H
#define ICARUS_AST_ACCESS_H

#include <string>
#include "ast/expression.h"

namespace AST {
struct Access : public Expression {
  ~Access() {}
  std::string to_string(size_t n) const override {
    return operand->to_string(n) + "." + member_name;
  }

  void assign_scope(Scope *scope) override;
  void ClearIdDecls() override;
  void VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, std::vector<IR::Val> *args) override {
    operand->SaveReferences(scope, args);
  }

  void ExtractReturns(std::vector<const Expression *> *rets) const {
    operand->ExtractReturns(rets);
  }

  void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) override;

  IR::Val EmitIR(Context *);
  IR::Val EmitLVal(Context *);

  Access *Clone() const override;
  std::string member_name;
  std::unique_ptr<Expression> operand;
};

}  // namespace AST

#endif  // ICARUS_AST_ACCESS_H
