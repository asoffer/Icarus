#ifndef ICARUS_AST_CHAINOP_H
#define ICARUS_AST_CHAINOP_H

#include <memory>
#include <vector>
#include "ast/dispatch.h"
#include "ast/expression.h"
#include "frontend/operators.h"

namespace AST {
struct ChainOp : public Expression {
  ~ChainOp() override {}

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

  ChainOp *Clone() const override;
  std::vector<Language::Operator> ops;
  std::vector<std::unique_ptr<Expression>> exprs;
  std::vector<DispatchTable> dispatch_tables_;
};
}  // namespace AST
#endif // ICARUS_AST_CHAINOP_H
