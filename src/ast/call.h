#ifndef ICARUS_AST_CALL_H
#define ICARUS_AST_CALL_H

#include "ast/dispatch.h"
#include "ast/expression.h"
#include "ast/fn_args.h"

namespace AST {
struct Call : public Expression {
  ~Call() override {}
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
  Call *Clone() const override;

  IR::Val EmitIR(Context *) override;
  IR::Val EmitLVal(Context *) override;

  std::unique_ptr<Expression> fn_;
  FnArgs<std::unique_ptr<Expression>> args_;

  // Filled in after type verification
  DispatchTable dispatch_table_;
};
}  // namespace AST

#endif  // ICARUS_AST_CALL_H